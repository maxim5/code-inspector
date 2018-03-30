-- Simple mapping between cal_items and assignments

create table evaluation_cal_task_map (
	task_item_id	integer
			constraint evaluation_cal_task_tid_fk
			references cr_items(item_id),
	cal_item_id	integer
			constraint evaluation_cal_task_cid_fk
			references cal_items(cal_item_id)
);

create index ev_cal_task_map_tcid_index on evaluation_cal_task_map(task_item_id,cal_item_id);

