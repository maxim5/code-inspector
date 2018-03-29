package servlet_homework;

import java.io.IOException;
import java.io.PrintWriter;
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
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
    
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String message = request.getParameter("message");
		String action = request.getParameter("action");
		String scope = request.getParameter("scope");
		String name = request.getParameter("name");
		String value = request.getParameter("value");
		
		HttpSession sessionContest =  request.getSession();
		ServletContext sevrletContest = request.getServletContext();
		
		if("session".equalsIgnoreCase(scope)){
			if("add".equalsIgnoreCase(action) || "update".equalsIgnoreCase(action)){
				sessionContest.setAttribute(name, value);
			} else if("remove".equalsIgnoreCase(action)) {
				sessionContest.removeAttribute(name);
			}
		} else if("servlet".equalsIgnoreCase(scope)) {
			if("add".equalsIgnoreCase(action) || "update".equalsIgnoreCase(action)){
				sevrletContest.setAttribute(name, value);
			} else if("remove".equalsIgnoreCase(action)) {
				sevrletContest.removeAttribute(name);
			}
		}
		
		PrintWriter out = response.getWriter();

		out.println("<!DOCTYPE html>");
		out.println("<head><title>Servlet</title></head>");
		out.println("<body>");
		out.println("	<form action='./Manage?' method='get'>");
		out.println("		<p> action <select name=action>");
		out.println("			<option value='add'>add</option>");
		out.println("			<option value='update'>update</option>");
		out.println("			<option value='remove'>remove</option>");
		out.println("		</select></p>");
		out.println("		<p> scope<select name=scope>");
		out.println("			<option value='session'>session</option>");
		out.println("			<option value='servlet'>servlet</option>");
		out.println("		</select></p>");
		out.println("		<p>message <input type=text name='message'></p>");
		out.println("		<p>name <input type=text name='name'></p>");
		out.println("		<p>value <input type=text name='value'></p>");
		out.println("		<input type=submit value='send'><input type=reset value='clear'>");
		out.println("	</form>");
		
		
		out.println("<p> Message : " + message + "</p>");
		
		out.println("<p><h4>Session contest:</h4></p>");
		
		Enumeration<String> names = sessionContest.getAttributeNames();
		while(names.hasMoreElements()) {
			String atribute = names.nextElement();
			out.println("<p>" + atribute + " : " + sessionContest.getAttribute(atribute) + "</p>");
		}
		
		response.getWriter().println("<p><h4>Servlet contest:</h4></p>");
		names = sevrletContest.getAttributeNames();
		while(names.hasMoreElements()) {
			String atribute = names.nextElement();
			out.println("<p>" + atribute + " : " +sevrletContest.getAttribute(atribute) + "</p>");
		}
		
		out.println("</body></html>");
		
		
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
	}

}
