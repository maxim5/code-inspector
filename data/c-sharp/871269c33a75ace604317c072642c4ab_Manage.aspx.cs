using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;

using Weekee.Emis.DAL;
using Weekee.Emis.Core;

namespace Weekee.Emis.Web.SystemManage
{
	/// <summary>
	/// Manage ??????
	/// </summary>
	public class Manage : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel Panel1;
		protected System.Web.UI.WebControls.Panel Panel2;
		protected System.Web.UI.WebControls.Label lasel;
		protected System.Web.UI.WebControls.TextBox txtExecute;
		protected System.Web.UI.WebControls.TextBox txtselect1;
		protected System.Web.UI.WebControls.TextBox txtselect2;
		protected System.Web.UI.WebControls.TextBox txtselect3;
		protected System.Web.UI.WebControls.Label lbcap;
		protected System.Web.UI.WebControls.Panel Panel3;
		protected System.Web.UI.WebControls.Button btnok;
		protected System.Web.UI.HtmlControls.HtmlTable selecttable3;
		protected System.Web.UI.HtmlControls.HtmlTable selecttable1;
		protected System.Web.UI.HtmlControls.HtmlTable selecttable2;


		protected DbExecute DbThis=new DbExecute();
	
		private void Page_Load(object sender, System.EventArgs e)
		{
			
			// ???????????????
		}


		/// <summary>
		/// ??SQL??
		/// </summary>

		private void ExecuteSql()
		{
			if (this.txtExecute.Text.Trim().ToString()!="")
			{
				try
				{ 
					this.lbcap.Text="?????:";

					DbThis.SqlText=this.txtExecute.Text.ToString();
				
					DbThis.ExecSqlText(true);

					this.lbcap.Text= DbThis.SqlText.ToString()+"????!";
					this.lbcap.Text="";

				}
				catch(Exception e)
				{
					this.lbcap.Text=e.Message.ToString();
				}
				
				finally
				{
               
				}
			}

			///????1:
			///
			if(this.txtselect1.Text.Trim().ToString()!="")
			{

				DataTable dt=new DataTable();

				this.selecttable1.Rows.Clear();
				HtmlTableRow htrHead=new HtmlTableRow();
				HtmlTableCell htc;
			 try
				{
                    DbThis.SqlText=this.txtselect1.Text.ToString();				   
				    dt= DbThis.SelectSqlText(true);
				    if(dt.Rows.Count>0)
				 {
					 for(int i=0;i<=dt.Columns.Count-1;i++) //?????
					 {
						
							 htc=new HtmlTableCell();
							 htc.InnerHtml="<b>"+dt.Columns[i].Caption.ToString()+"</b>";
							 htrHead.Cells.Add(htc);
							 this.selecttable1.Rows.Add(htrHead);
							                          
					 }

					 foreach(DataRow dr in dt.Rows)
					 {
                            //????
						 htc=new HtmlTableCell();
						 htc.InnerHtml="<tr><td align=left nowrap> </td></tr> ";
						 htrHead.Cells.Add(htc);
						 this.selecttable1.Rows.Add(htrHead);

                      for(int i=0;i<=dt.Columns.Count-1;i++)    //????:
					    
 					
						{
							htc=new HtmlTableCell();
							htc.InnerHtml=dr[i].ToString();
							htrHead.Cells.Add(htc);
							this.selecttable1.Rows.Add(htrHead);
						}     
					 }                         
				 }
                  
   				}
				catch(Exception e)
				{
					this.lasel.Text=e.Message.ToString();

				}
				finally
				{
				}

			}



            //????2



			if(this.txtselect2.Text.Trim().ToString()!="")
			{

				DataTable dt=new DataTable();

				this.selecttable2.Rows.Clear();
				HtmlTableRow htrHead=new HtmlTableRow();
				HtmlTableCell htc;
				try
				{
					DbThis.SqlText=this.txtselect2.Text.ToString();				   
					dt= DbThis.SelectSqlText(true);
					if(dt.Rows.Count>0)
					{
						for(int i=0;i<=dt.Columns.Count-1;i++) //?????
						{
						
							htc=new HtmlTableCell();
							htc.InnerHtml="<b>"+dt.Columns[i].Caption.ToString()+"</b>";
							htrHead.Cells.Add(htc);
							this.selecttable2.Rows.Add(htrHead);
							                          
						}

                    
						// foreach(dr in)
						// htc.InnerHtml="<td >"+dr[i].ToString()+"</td>";
						foreach(DataRow dr in dt.Rows)
						{
							//????
							htc=new HtmlTableCell();
							htc.InnerHtml="<tr><td align=left nowrap> </td></tr> ";
							htrHead.Cells.Add(htc);
							this.selecttable2.Rows.Add(htrHead);

							for(int i=0;i<=dt.Columns.Count-1;i++)    //????:
					    
 					
							{
								htc=new HtmlTableCell();
								htc.InnerHtml=dr[i].ToString();
								htrHead.Cells.Add(htc);
								this.selecttable2.Rows.Add(htrHead);
							}     
						}                         
					}
                  
				}
				catch(Exception e)
				{
					this.lasel.Text=e.Message.ToString();

				}
				finally
				{
				}

			}


			//????3



			if(this.txtselect3.Text.Trim().ToString()!="")
			{

				DataTable dt=new DataTable();

				this.selecttable3.Rows.Clear();
				HtmlTableRow htrHead=new HtmlTableRow();
				HtmlTableCell htc;
				try
				{
					DbThis.SqlText=this.txtselect3.Text.ToString();				   
					dt= DbThis.SelectSqlText(true);
					if(dt.Rows.Count>0)
					{
						for(int i=0;i<=dt.Columns.Count-1;i++) //?????
						{
						
							htc=new HtmlTableCell();
							htc.InnerHtml="<b>"+dt.Columns[i].Caption.ToString()+"</b>";
							htrHead.Cells.Add(htc);
							this.selecttable3.Rows.Add(htrHead);
							                          
						}

                    
						// foreach(dr in)
						// htc.InnerHtml="<td >"+dr[i].ToString()+"</td>";
						foreach(DataRow dr in dt.Rows)
						{
							//????
							htc=new HtmlTableCell();
							htc.InnerHtml="<tr><td align=left nowrap> </td></tr> ";
							htrHead.Cells.Add(htc);
							this.selecttable3.Rows.Add(htrHead);

							for(int i=0;i<=dt.Columns.Count-1;i++)    //????:
					    
 					
							{
								htc=new HtmlTableCell();
								htc.InnerHtml=dr[i].ToString();
								htrHead.Cells.Add(htc);
								this.selecttable3.Rows.Add(htrHead);
							}     
						}                         
					}
                  
				}
				catch(Exception e)
				{
					this.lasel.Text=e.Message.ToString();

				}
				finally
				{
				}

			}



		}


		#region Web ??????????
		override protected void OnInit(EventArgs e)
		{
			//
			// CODEGEN: ???? ASP.NET Web ??????????
			//
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// ?????????? - ???????????
		/// ???????
		/// </summary>
		private void InitializeComponent()
		{    
			this.btnok.Click += new System.EventHandler(this.btnok_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		private void btnok_Click(object sender, System.EventArgs e)
		{
			this.ExecuteSql();


		}

		
	}
}
