package com.msmeik.store.relish.setmeal.mvc;

import java.util.Date;
import java.util.List;

import com.msmeik.store.Store;
import com.msmeik.store.relish.RelishService;
import com.msmeik.store.relish.SetMealCategory;

public class Manage {
	private RelishService relishService;
	private List setMealList;
	private int page = 1;
	private int pageSize = 20;
	private int totalCount;
	private int storeId;
	private int categoryId = -1;
	private String orderBy;
	private List categoryList;
	private int status = -1;
	private String p;
	private int v;
	private int setMealId;
	private int ok;
	private Date orderTime;
	
	public String html(){
		Store store = new Store();
		store.setId(storeId);
		SetMealCategory category = new SetMealCategory();
		category.setId(categoryId);
		this.categoryList = this.relishService.getSetMealCategoryList(store);
		this.setMealList = this.relishService.getSetMealList(store, category, status, page, pageSize, orderBy);
		this.totalCount = this.relishService.getSetMealNum(store,category, status);
		return "success";
	}
	
	//设置所属套餐分类
	public String manage(){
		Store store = new Store();
		store.setId(storeId);
		if(p.equals("category")){
			SetMealCategory category = new SetMealCategory();
			category.setId(v);			
			this.ok = this.relishService.setSetMealCategory(store, category, setMealId);
		}
		if(p.equals("onTop")){
			this.ok = this.relishService.setSetMealOnTop(store, setMealId, v);
		}
		if(p.equals("recommend")){
			this.ok = this.relishService.setSetMealRecommend(store, setMealId, v);
		}
		if(p.equals("facia")){
			this.ok = this.relishService.setSetMealFacia(store, setMealId, v);
		}
		if(p.equals("status")){
			this.ok = this.relishService.setSetMealStatus(store, setMealId, v);
		}
		if(p.equals("orderTime")){
			this.orderTime = new Date();
			this.ok = this.relishService.setSetMealOrderTime(store, setMealId,orderTime);
		}
		return "success";
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public int getPageSize() {
		return pageSize;
	}

	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}

	public int getStoreId() {
		return storeId;
	}

	public void setStoreId(int storeId) {
		this.storeId = storeId;
	}

	public int getCategoryId() {
		return categoryId;
	}

	public void setCategoryId(int categoryId) {
		this.categoryId = categoryId;
	}

	public String getOrderBy() {
		return orderBy;
	}

	public void setOrderBy(String orderBy) {
		this.orderBy = orderBy;
	}

	public List getSetMealList() {
		return setMealList;
	}

	public int getTotalCount() {
		return totalCount;
	}

	public List getCategoryList() {
		return categoryList;
	}

	public void setRelishService(RelishService relishService) {
		this.relishService = relishService;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public int getV() {
		return v;
	}

	public void setV(int v) {
		this.v = v;
	}

	public int getSetMealId() {
		return setMealId;
	}

	public void setSetMealId(int setMealId) {
		this.setMealId = setMealId;
	}

	public int getOk() {
		return ok;
	}

	public void setP(String p) {
		this.p = p;
	}

	public Date getOrderTime() {
		return orderTime;
	}
	
	
}
