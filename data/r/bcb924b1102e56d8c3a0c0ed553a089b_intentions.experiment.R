source("loader/load_items.R")
source("loader/load_intentions.R")
source("loader/load_hides.R")
source("loader/load_found.R")
items = load_items(reload=T)
intentions = load_intentions(reload=T)

library(doBy)
library(ggplot2)

bool_or = function(x){
	sum(x) > 0
}
bool_and = function(x){
	sum(x) == length(x)
}

#Collapses multiple ratings by one reviewer into a single rating
coder.items = with(
	summaryBy(
		utility + unsure ~ feedback_id + chunk_id + bucket_id + sample + af_link_id,
		data=items,
		FUN=c(bool_or)
	),
	data.frame(
		feedback_id = feedback_id,
		chunk_id    = chunk_id,
		sample      = sample,
		prominent   = af_link_id == 4,
		bucket_id   = bucket_id,
		utility     = utility.bool_or,
		unsure      = unsure.bool_or
	)
)

getIntentionBool = function(feedback_id, chunk_id, intention){
	ints = intentions[
		intentions$chunk_id    == as.character(chunk_id) &
		intentions$feedback_id == feedback_id &
		intentions$value       == intention,
	]$value
	length(ints) > 0
}

coder.items$abuse      = with(coder.items, mapply(getIntentionBool, feedback_id, chunk_id, "abuse"))
coder.items$irrelevant = with(coder.items, mapply(getIntentionBool, feedback_id, chunk_id, "irrelevant"))
coder.items$issue      = with(coder.items, mapply(getIntentionBool, feedback_id, chunk_id, "issue"))
coder.items$praise     = with(coder.items, mapply(getIntentionBool, feedback_id, chunk_id, "praise"))
coder.items$question   = with(coder.items, mapply(getIntentionBool, feedback_id, chunk_id, "question"))
coder.items$suggestion = with(coder.items, mapply(getIntentionBool, feedback_id, chunk_id, "suggestion"))

intention.items = with(
	summaryBy(
		abuse + irrelevant + issue + praise + question + suggestion + utility ~ feedback_id + bucket_id + sample,
		data=coder.items,
		FUN=c(bool_or, bool_and)
	),
	rbind(
		data.frame(
			feedback_id = feedback_id,
			bucket_id   = bucket_id,
			sample      = sample,
			utility     = utility.bool_and,
			intention   = "abuse",
			categorized = abuse.bool_or
		),
		data.frame(
			feedback_id = feedback_id,
			bucket_id   = bucket_id,
			sample      = sample,
			utility     = utility.bool_and,
			intention   = "irrelevant",
			categorized = irrelevant.bool_or
		),
		data.frame(
			feedback_id = feedback_id,
			bucket_id   = bucket_id,
			sample      = sample,
			utility     = utility.bool_and,
			intention   = "issue",
			categorized = issue.bool_or
		),
		data.frame(
			feedback_id = feedback_id,
			bucket_id   = bucket_id,
			sample      = sample,
			utility     = utility.bool_and,
			intention   = "praise",
			categorized = praise.bool_or
		),
		data.frame(
			feedback_id = feedback_id,
			bucket_id   = bucket_id,
			sample      = sample,
			utility     = utility.bool_and,
			intention   = "question",
			categorized = question.bool_or
		),
		data.frame(
			feedback_id = feedback_id,
			bucket_id   = bucket_id,
			sample      = sample,
			utility     = utility.bool_and,
			intention   = "suggestion",
			categorized = suggestion.bool_or
		)
	)
)

intention.agg = with(
	summaryBy(
		utility ~ utility + bucket_id + intention + categorized,
		data=intention.items[intention.items$sample == "12-27" | intention.items$sample == "01-09",],
		FUN=c(length)
	),
	data.frame(
		bucket_id   = bucket_id,
		intention   = intention,
		categorized = categorized,
		utility     = utility,
		count       = utility.length
	)
)
intention.agg = transformBy(
	~ bucket_id + intention,
	data = intention.agg,
	total=sum(count),
	prop =count/sum(count)
)
intention.agg$utility = as.factor(sapply(intention.agg$utility, function(utility){if(utility){"useful"}else{"useless"}}))

png("plots/intention_prop.by_interface.by_utility.png", width=2048, height=600, res=225)
ggplot(
	intention.agg[intention.agg$categorized,], 
	aes(x=as.character(bucket_id), y=prop)
) + 
geom_bar(
	stat="identity", 
	aes(fill=utility),
	color="#000000"
) + 
facet_wrap(~ intention, ncol=6) + 
scale_fill_manual(
	name="", 
	values=c("useful"="#66FF66", "useless"="#BBBBBB")
) + 
scale_x_discrete("Interface") + 
scale_y_continuous("Proportion of feedback") +
theme_bw()
dev.off()

intention.utility = with(
	summaryBy(
		utility ~ intention + bucket_id,
		data=intention.items[
			(
				intention.items$sample == "12-27" | 
				intention.items$sample == "01-09"
			) &
			intention.items$categorized,
		],
		FUN=c(sum, length)
	),
	data.frame(
		intention,
		bucket_id,
		useful = utility.sum,
		prop   = utility.sum/utility.length,
		n      = utility.length
	)
)
intention.utility$se = with(
	intention.utility,
	sqrt(prop*(1-prop)/n)
)

png("plots/utility_prop.by_intention.by_experiment.png", width=1600, height=1600, res=300)
ggplot(
	intention.utility, 
	aes(x=intention, y=prop)
) + 
geom_bar(
	stat="identity", 
	color="#000000",
	position=position_dodge(width=0.9),
	aes(fill=as.factor(bucket_id))
) + 
geom_errorbar(
	aes(
		ymax=prop + se,
		ymin=prop - se
	),
	width=0.5,
	position=position_dodge(width=0.9),
) + 
scale_fill_discrete("Interface") +
scale_x_discrete("\nFeedback type") + 
scale_y_continuous("Useful proportion\n") +
theme_bw() #+ 
#geom_text(
#	aes(
#		y=conditional(prop > 0.2, 0.02, prop+se+0.02),
#		label=round(prop, 3)
#	)
#)
dev.off()

intention.intended = with(
	summaryBy(
		categorized ~ intention + bucket_id,
		data=intention.items[
			(
				intention.items$sample == "12-27" | 
				intention.items$sample == "01-09"
			),
		],
		FUN=c(sum, length)
	),
	data.frame(
		intention,
		bucket_id,
		count  = categorized.sum,
		prop   = categorized.sum/categorized.length,
		n      = categorized.length
	)
)
intention.intended$se = with(
	intention.intended,
	sqrt(prop*(1-prop)/n)
)

png("plots/intention_prop.by_experiment.png", width=1600, height=1600, res=300)
ggplot(
	intention.intended, 
	aes(x=intention, y=prop)
) + 
geom_bar(
	stat="identity", 
	color="#000000",
	position=position_dodge(width=0.9),
	aes(fill=as.factor(bucket_id))
) + 
geom_errorbar(
	aes(
		ymax=prop + se,
		ymin=prop - se
	),
	position=position_dodge(width=0.9),
	width=0.5
) + 
scale_fill_discrete("Interface") +
scale_x_discrete("\nFeedback type") + 
scale_y_continuous("Proportion of submissions\n") +
theme_bw() #+ 
#geom_text(
#	aes(
#		y=conditional(prop > 0.05, 0.01, prop+se+0.01),
#		label=round(prop, 3)
#	)
#)
dev.off()
