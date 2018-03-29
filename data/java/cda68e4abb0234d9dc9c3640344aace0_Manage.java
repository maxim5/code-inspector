package Manage;

import java.io.IOException;
import java.util.Enumeration;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * Servlet implementation class Manage
 */
public class Manage extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public Manage() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		writeHTML(response);
		String message = request.getParameter("message");
		if(message != null){
			response.getWriter().write("<hr><br><center>Message:<br>" + message + "<br>");
		}
		getParamerersFromLinkAndSetToScope(request, response);
	}
	
	/**
	 * Method prints html page
	 * @param response
	 */
	public void writeHTML(HttpServletResponse response) {
		String str = "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN' 'http://www.w3.org/TR/html4/loose.dtd'>"
				+ "<html>"
				+ "<head>"
				+ "<title>GeekHub</title>"
				+ "</head>"
				+ "<body>"
				+ "<center>"
				+ "<h1>Lesson 7. GeekHub</h1>"
				+ "<form>"
				+ "<table border = 1 height = 500 width = 1000>"
				+ "<tr>"
				+ "<th>Action:</th>"
				+ "<td>"
				+ "<input type='radio' name='action' value='add' checked='checked'>Add</td>"
				+ "<td>"
				+ "<input type='radio' name='action' value='update'>Update</td>"
				+ "<td>"
				+ "<input type='radio' name='action' value='remove'>Remove</td>"
				+ "<td>"
				+ "<input type='radio' name='action' value='invalidate'>Invalidate</td>"
				+ "</tr>"
				+ "<tr>"
				+ "<th>Scope:</th>"
				+ "<td colspan = 2><input type='radio' name='scope' value='session' checked='checked'>Session</td>"
				+ "<td colspan = 2><input type='radio' name='scope' value='app'>ServerContext</td>"
				+ "</tr>"
				+ "<tr>"
				+ "<th>Name:</th>"
				+ "<td colspan = 4><input type = 'text' name = 'name' value = ''></td>"
				+ "</tr>"
				+ "<tr>"
				+ "<th>Value:</th>"
				+ "<td colspan = 4>"
				+ "<input type = 'text' name = 'value' value = ''></td>"
				+ "</tr>"
				+ "<tr>"
				+ "<th>Message:</th>"
				+ "<td colspan = 4><textarea rows=3 cols=45 name='message'></textarea></td>"
				+ "</tr>"
				+ "<tr>"
				+ "<td colspan = 5><input type = 'submit' value = 'Submit Button'></td>"
				+ "</tr>"
				+ "</table>"
				+ "</form>"
				+ "</center>"
				+ "</body>"
				+ "</html>";
		try {
			response.getWriter().write(str);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Method receives all parameters by method GET initializes variables and set values
	 * into session and servler context
	 * @param request
	 * @param response
	 */
	public void getParamerersFromLinkAndSetToScope(HttpServletRequest request, HttpServletResponse response){
		String action = request.getParameter("action");
		String name = request.getParameter("name");
		String value = request.getParameter("value");
		String scope = request.getParameter("scope");
		HttpSession session = request.getSession();
		ServletContext context = request.getServletContext();
		boolean invalidateStatus = false;
		if (scope != null) {
			if ("session".equals(scope)) {
				if ("invalidate".equals(action)) {
					session.invalidate();
					invalidateStatus = true;
				} else if (name != null) {
					if ("add".equals(action) || "update".equals(action)
							&& value != null) {
						session.setAttribute(name, value);
					} else if ("remove".equals(action)) {
						session.removeAttribute(name);
					}
				}
			} else if ("app".equals(scope)) {
				if (name != null) {
					if ("add".equals(context) || "update".equals(context)
							&& value != null) {
						context.setAttribute(name, value);
					} else if ("remove".equals(context)) {
						context.removeAttribute(name);
					}
				}
			}
		}
		getAndWriteSessionParameters(response, session, invalidateStatus);
		getAndWriteContextParameters(response, context);
	}
	
	/**
	 * Method prints all session parameters
	 * @param response
	 * @param session
	 * @param invalidateStatus
	 */
	public void getAndWriteSessionParameters(HttpServletResponse response,
			HttpSession session, boolean invalidateStatus) {
		String resultStr = "";
		if (invalidateStatus == false) {
			resultStr = "<hr><br>Session:"+"<br>";
			@SuppressWarnings("rawtypes")
			Enumeration valuesSession = session.getAttributeNames();
			while (valuesSession.hasMoreElements()) {
				String key = (String) valuesSession.nextElement();
				String val = (String) session.getAttribute(key);
				if(key != null && val != null){
					resultStr += key + " - " + val + "<br>";
				}
			}
			try {
				response.getWriter().write(resultStr);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Method prints all servlet context parameters
	 * @param response
	 * @param context
	 */
	public void getAndWriteContextParameters(HttpServletResponse response, ServletContext context){
		String resultStr = "<hr><br>Context:"+"<br>";
		@SuppressWarnings("rawtypes")
		Enumeration valuesContext = context.getAttributeNames();
		while (valuesContext.hasMoreElements()) {
			String key = (String) valuesContext.nextElement();
			String val = (String) context.getAttribute(key).toString();
			if(key != null && val != null){
				resultStr += key + " - " + val + "<br>";
			}
		}
		try {
			response.getWriter().write(resultStr + "<hr></center>");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
