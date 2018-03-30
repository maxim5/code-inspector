/*
 * combine zero vectors with classifier predictions 
 * for a 'complete' truth table w/ ir metrics
 * cleanup anything that didn't come from libsvm
 */
delete ir
from classifier_eval_ir ir
inner join classifier_eval e on ir.classifier_eval_id = e.classifier_eval_id
where e.experiment = '@kernel.experiment@'
and e.name = '@kernel.name@'
and e.algorithm = '@kernel.algo@'
and ir.ir_type <> ''
and e.fold > 0
and e.run > 0
;


drop table if exists hzv_cutoff;
create temporary table hzv_cutoff
as
select distinct param1 rank
from classifier_eval
where name = '@kernel.name@' 
and experiment = '@kernel.experiment@'
;
create unique index IX_rank on  hzv_cutoff(rank);

drop table if exists hzv_tt;
/**
 * per-fold truth table for zero vectors induced by specified cutoffs.
 * we simply add this truth table to the truth table of the classifier.
 */
create temporary table hzv_tt
as
select label, run, fold, ir_class, cutoff, tp, tn, fp, fn
from
(
	/* truth table */
	select label, cutoff, ir_class, run, fold,
	  sum(case
	    when ir_class = target_class and ir_class = 'N' then 1
	    else 0
	  end) tp,
	  sum(case
	    when ir_class <> target_class and ir_class <> 'N' then 1
	    else 0
	  end) tn,
	  sum(case
	    when ir_class <> target_class and ir_class = 'N' then 1
	    else 0
	  end) fp,
	  sum(case
	    when ir_class = target_class and ir_class <> 'N' then 1
	    else 0
	  end) fn
	from
	(
		select hi.label, ce.run, ce.fold, hc.rank cutoff, hi.instance_id, ir_class, l.class target_class
		from hotspot_instance hi
		/* limit to zero vectors */
		inner join hzv_cutoff hc on hi.min_rank > hc.rank
		/* join with gold class */
		inner join corpus_label l 
			on l.instance_id = hi.instance_id
			and l.corpus_name = hi.corpus_name
			and l.label = hi.label
	    inner join cv_fold ce 
	    	on ce.label = hi.label
	    	and ce.corpus_name = hi.corpus_name
	    /* limit to the test instances of this fold */
	    inner join cv_fold_instance ci
	      on ci.cv_fold_id = ce.cv_fold_id
	      and ci.instance_id = hi.instance_id
	      and ci.train = 0
		inner join
		/* create truth table - get all unique class ids */
		(
			select label, class ir_class
			from v_corpus_group_class
			where corpus_name = '@kernel.name@'
			and doc_group = 'train'
		) ja on ja.label = l.label
		where hi.corpus_name = '@kernel.name@'
		and hi.experiment = '@kernel.hzv.experiment@'
	) s
	group by label, run, fold, ir_class, cutoff
) s;

create unique index NK_hzv_tt on hzv_tt(label, run, fold, ir_class, cutoff);

/* sanity check this should not return any rows */
select *
from
(
    -- get min and max - they should be identical
    select label, run, fold, cutoff, min(tot) mt, max(tot) xt
    from
    (
    	-- get number of instances per fold/class combo
	    select label, run, fold, ir_class, cutoff, tp+tn+fp+fn tot
	    from hzv_tt
    ) s
    group by label, run, fold, cutoff
) s where mt <> xt
;

/**
 * for some hotspot cutoffs + folds, we may be missing entire classes
 * add the missing truth tables
 */
drop table if exists tmp_missing_ir;
create temporary table tmp_missing_ir
as
select f.cv_fold_id, f.label, fc.class, e.classifier_eval_id, e.param1
from v_corpus_group_class fc
inner join cv_fold f 
    on f.label = fc.label 
    and f.corpus_name = '@kernel.name@'
inner join classifier_eval e
    on e.name = f.corpus_name
    and e.experiment = '@kernel.experiment@'
    and e.algorithm = '@kernel.algo@'
    and e.label = f.label
    and e.run = f.run
    and e.fold = f.fold
/* filter out the class ids for which we have the ir metrics */
left join classifier_eval_ir ir 
    on ir.classifier_eval_id = e.classifier_eval_id 
    and ir.ir_class = fc.class
where ir.classifier_eval_ir_id is null
and fc.doc_group = 'train'
;

create unique index NK_tmp_missing_ir on tmp_missing_ir (cv_fold_id, label, class, classifier_eval_id, param1);

insert into classifier_eval_ir (classifier_eval_id, ir_class, tn, fn, ir_class_id, tp, fp, ir_type)
select *, 0, 0, 0, 'miss'
from
(
	select fc.classifier_eval_id, fc.class, sum(a.class <> fc.class) tn, sum(a.class = fc.class) fn
	from
	/* get fold, classifier evaluation, and missing class ids */
	tmp_missing_ir fc 
	/* get the test instances for this fold */
	inner join cv_fold_instance fi 
	    on fi.cv_fold_id = fc.cv_fold_id 
	    and fi.train = 0
	/* get the judgement for these instances */
	inner join corpus_label a 
	    on a.instance_id = fi.instance_id 
	    and a.label = fc.label
	    and a.corpus_name = '@kernel.name@'
	/* filter out zero vectors */
	inner join hotspot_instance i 
	    on i.experiment = '@kernel.hzv.experiment@'
	    and i.corpus_name = a.corpus_name
	    and i.label = fc.label 
	    and i.instance_id = fi.instance_id
	    and i.min_rank <= param1
	where a.corpus_name = '@kernel.name@'
	group by fc.classifier_eval_id, fc.class
) s
;

/*
 * combined classifier + zero vector truth table and ir metrics
 * the zero vectors for one experiment (kernel.hzv.experiment) may be applicable to the zero vectors
 * for another experiment (kernel.experiment), 
 */
insert into classifier_eval_ir (classifier_eval_id, ir_class, ir_class_id, tp, tn, fp, fn, ppv, npv, sens, spec, f1, ir_type)
select s.*,
  case when ppv + sens > 0 then 2*ppv*sens/(ppv+sens) else 0 end f1, 'zv'
from
(
	select s.*,
	  case when tp+fp <> 0 then tp/(tp+fp) else 0 end ppv,
	  case when tn+fn <> 0 then tn/(tn+fn) else 0 end npv,
	  case when tp+fn <> 0 then tp/(tp+fn) else 0 end sens,
	  case when tn+fp <> 0 then tn/(tn+fp) else 0 end spec
	from
	(
		select
		  e.classifier_eval_id,
		  ir.ir_class,
		  ir.ir_class_id,
		  ir.tp + coalesce(z.tp, 0) tp,
		  ir.tn + coalesce(z.tn, 0) tn,
		  ir.fp + coalesce(z.fp, 0) fp,
		  ir.fn + coalesce(z.fn, 0) fn
		from classifier_eval_ir ir
		inner join classifier_eval e 
			on ir.classifier_eval_id = e.classifier_eval_id
			and e.experiment = '@kernel.experiment@'
			and e.name = '@kernel.name@'
			and e.algorithm = '@kernel.algo@'
			and e.run > 0
			and e.fold > 0
		left join hzv_tt z
			on (z.label, z.run, z.fold, z.ir_class, z.cutoff) = (e.label, e.run, e.fold, ir.ir_class, e.param1)
		where ir.ir_type in ('', 'miss')
	) s
) s
;

/*
sanity check all - this should return 0
make sure the number of instances for each class is the same
*/
select *
from
(
    -- get min and max - they should be identical
    select label, run, fold, param1, min(tot) mt, max(tot) xt
    from
    (
    	-- get number of instances per fold/class combo
	    select label, run, fold, ir_class_id, param1, tp+tn+fp+fn tot
	    from classifier_eval_ir ir
	    inner join classifier_eval e on ir.classifier_eval_id = e.classifier_eval_id 
	    where name = '@kernel.name@' 
		and experiment = '@kernel.experiment@'
		and e.algorithm = '@kernel.algo@'
		and ir_type = 'zv'
	    and run > 0
	    and fold > 0
    ) s
    group by label, run, fold, param1
) s where mt <> xt
;

/*
sanity check - this should return no rows
make sure the number of instances in the test fold is identical
to the number of instances in our truth table
*/
select e.*, (tp+tn+fp+fn) tot, fc
from classifier_eval_ir t
inner join classifier_eval e on e.classifier_eval_id = t.classifier_eval_id
inner join
(
	/* count up instances per test fold */
    select corpus_name, label, run, fold, count(*) fc
    from cv_fold f
    inner join cv_fold_instance i 
        on f.cv_fold_id = i.cv_fold_id
        and i.train = 0
    group by corpus_name, label, run, fold
) cv on cv.corpus_name = e.name
    and e.label = cv.label 
    and e.run = cv.run 
    and e.fold = cv.fold
where e.experiment = '@kernel.experiment@'
and e.name = '@kernel.name@' 
and e.algorithm = '@kernel.algo@' 
and e.run > 0
and e.fold > 0
and (tp+tn+fp+fn) <> fc
and t.ir_type = 'zv'
;

/*
 * show the best f1 per label for the experiment
 */
select label, truncate(max(f1),3) f1
from
(
	/*
	 * best f1 score by experiment (hotspot cutoff) and svm parameters
	 */
	select label, kernel, cost, gamma, weight, param1, param2, avg(f1) f1
	from classifier_eval_ir t
	inner join classifier_eval e on e.classifier_eval_id = t.classifier_eval_id
	inner join classifier_eval_svm l on e.classifier_eval_id = l.classifier_eval_id
	where experiment = '@kernel.experiment@'
	and name = '@kernel.name@'
	and e.algorithm = '@kernel.algo@'
	and run > 0
	and fold > 0
	and t.ir_type = 'zv'
	group by label, kernel, cost, gamma, weight, param1, param2
) s
group by label
order by label
;