/**   
 * @Title: Manage.java
 * @Package rjzjh.tech_mainWeb.pages.wlWeb.order
 * @Description: TODO(用一句话描述该文件做什么)
 * @author 周俊辉  
 * @date 2010-12-14 上午10:16:56
 * @version V1.0   
 */
package rjzjh.tech_mainWeb.pages.wlWeb.order;

import java.util.ArrayList;
import java.util.List;

import org.apache.tapestry5.Block;
import org.apache.tapestry5.RenderSupport;
import org.apache.tapestry5.ajax.MultiZoneUpdate;
import org.apache.tapestry5.annotations.Component;
import org.apache.tapestry5.annotations.Environmental;
import org.apache.tapestry5.annotations.Events;
import org.apache.tapestry5.annotations.OnEvent;
import org.apache.tapestry5.annotations.Property;
import org.apache.tapestry5.corelib.components.Form;
import org.apache.tapestry5.corelib.components.Zone;
import org.apache.tapestry5.ioc.annotations.Inject;

import rjzjh.tech.dao.bean.constant.OrderState;
import rjzjh.tech.dao.bean.wl.OrderUser;
import rjzjh.tech.dao.itf.IUserDao;
import rjzjh.tech.service.wl.IUserService;

/**
 * @ClassName: Manage
 * @Description: TODO(这里用一句话描述这个类的作用)
 * @author 周俊辉
 * @date 2010-12-14 上午10:16:56
 * 
 */
public class Manage {
	@Inject
	private IUserService userService;
	@Environmental
	private RenderSupport renderSupport;

	@Property
	private String orderNo;

	@Property
	private OrderUser orderUser;
	
	@Property
	private OrderUser updateObj=new OrderUser();

	@Property
	private List<OrderUser> orderUsers;

	@Inject
	private Block viewBlock;
	@Component
	private Form editForm;
	@Component 
	private Zone loadZone;
	
	@Property
	private int updateObj_id;
	
	@Property
	private String updateObj_orderName;
	
	@Property
	private String updateObj_orderNo;
	
	@Property
	private OrderState updateObj_state;
	
	

	List<String> onProvideCompletionsFromOrderNo(String orderNo) {
		List<String> returnOrderNos = userService.findOrderNos(orderNo);
		return returnOrderNos;
	}

	Block onSuccessFromLoadForm() {
		orderUsers = userService.findOrderByOrderNo(orderNo);
		return viewBlock;
	}
	

	

	Form onActionFromEdit(int orderId) {
		orderUser = userService.loadOrderUser(orderId);
		updateObj_id = orderUser.getId();
		updateObj_orderNo = orderUser.getOrderNo();
		updateObj_orderName = orderUser.getOrderName();
		updateObj_state = orderUser.getState();
		return editForm;
	}

	Object onSuccessFromEditForm() {
		OrderUser updateObj = userService.loadOrderUser(updateObj_id);
		updateObj.setOrderNo(updateObj_orderNo);
		updateObj.setOrderName(updateObj_orderName);
		updateObj.setState(updateObj_state);
		userService.updateOrderUser(updateObj);
		orderUsers = new ArrayList<OrderUser>();
		orderUsers.add(updateObj);
		
		orderNo = updateObj_orderNo;
		//return viewBlock;
		return new MultiZoneUpdate("theZone", viewBlock).add("loadZone", loadZone.getBody());
	}
	public OrderState getState_new() {
		return OrderState.NEW;
	}

	public OrderState getState_sendgood() {
		return OrderState.SENDGOOD;
	}

	public OrderState getState_signin() {
		return OrderState.SIGNIN;
	}

	public OrderState getState_finsh() {
		return OrderState.FINISH;
	}
}
