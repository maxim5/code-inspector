package Manage;

import java.io.IOException;
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
		
		getParamerersFromLinkAndSetToScope(request);
		String param = request.getParameter("cd");
		String jspName = "noTagLib".equals(param) ? "default.jsp" : "index.jsp";
		request.getRequestDispatcher(jspName).forward(request, response);
	}
	
	/**
	 * Method receives all parameters by method GET initializes variables and set values
	 * into session and servler context
	 * @param request
	 * @param response
	 */
	private void getParamerersFromLinkAndSetToScope(HttpServletRequest request){
		String action = request.getParameter("action");
		String name = request.getParameter("name");
		String value = request.getParameter("value");
		HttpSession session = request.getSession();
		if (name != null) {
			if ("remove".equals(action)) {
				session.removeAttribute(name);
			} else if(value != null) {
				session.setAttribute(name, value);
			}
		}
	}
}