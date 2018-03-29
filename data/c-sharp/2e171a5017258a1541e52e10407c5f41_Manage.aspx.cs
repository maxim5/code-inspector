using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using Microsoft.AspNet.Membership.OpenAuth;

public partial class Account_Manage : System.Web.UI.Page
{
    protected string SuccessMessage
    {
        get;
        private set;
    }

    protected bool CanRemoveExternalLogins
    {
        get;
        private set;
    }

    protected override void OnPreRender(EventArgs e)
    {
        base.OnPreRender(e);
        init();
    }

    
    protected void Page_Load()
    {
        if (!IsPostBack)
        {
            // Determine the sections to render
            var hasLocalPassword = OpenAuth.HasLocalPassword(User.Identity.Name);
            setPassword.Visible = !hasLocalPassword;
            changePassword.Visible = hasLocalPassword;

            CanRemoveExternalLogins = hasLocalPassword;

            // Render success message
            var message = Request.QueryString["m"];
            if (message != null)
            {
                // Strip the query string from action
                Form.Action = ResolveUrl("~/Account/Manage.aspx");

                SuccessMessage =
                    message == "ChangePwdSuccess" ? "Your password has been changed."
                    : message == "SetPwdSuccess" ? "Your password has been set."
                    : message == "RemoveLoginSuccess" ? "The external login was removed."
                    : String.Empty;
                successMessage.Visible = !String.IsNullOrEmpty(SuccessMessage);
            }
        }
        //init();
    }

    protected void init()
    {
        fillRegiao();
        if (Profile.Idade.Length > 0)
        {
           // System.Diagnostics.Debug.Write("IDADE:" + Profile.Idade + "\n");
             TIdade.Text = Profile.Idade;
        }

        if (Profile.Regiao.Length > 0)
        {
           // System.Diagnostics.Debug.Write("REGIAO:" + Profile.Regiao + "\n");
            RegiaoActual.Text = "Actual: " + Profile.Regiao;
            RegiaoActual.Visible = true;
        }
    }
    protected void setPassword_Click(object sender, EventArgs e)
    {
        if (IsValid)
        {
            var result = OpenAuth.AddLocalPassword(User.Identity.Name, password.Text);
            if (result.IsSuccessful)
            {
                Response.Redirect("~/Account/Manage.aspx?m=SetPwdSuccess");
            }
            else
            {
                
                ModelState.AddModelError("NewPassword", result.ErrorMessage);
                
            }
        }
    }

    
    public IEnumerable<OpenAuthAccountData> GetExternalLogins()
    {
        var accounts = OpenAuth.GetAccountsForUser(User.Identity.Name);
        CanRemoveExternalLogins = CanRemoveExternalLogins || accounts.Count() > 1;
        return accounts;
    }

    public void RemoveExternalLogin(string providerName, string providerUserId)
    {
        var m = OpenAuth.DeleteAccount(User.Identity.Name, providerName, providerUserId)
            ? "?m=RemoveLoginSuccess"
            : String.Empty;
        Response.Redirect("~/Account/Manage.aspx" + m);
    }
    

    protected static string ConvertToDisplayDateTime(DateTime? utcDateTime)
    {
         return utcDateTime.HasValue ? utcDateTime.Value.ToLocalTime().ToString("G") : "[never]";
    }

    protected void setIdade_click(object sender, EventArgs e)
    {
        //System.Diagnostics.Debug.Write("TEXCT:" + TIdade.Text);
        Profile.Idade = TIdade.Text;
        
        //Response.Redirect("~/Account/Manage.aspx");
    }
    
    protected void setRegiao_click(object sender, EventArgs e)
    {
        Profile.Regiao = DDLRegiao.SelectedItem.Text;
       // Response.Redirect("~/Account/Manage.aspx");
    }
    protected void fillRegiao()
    {
        var ddl = DDLRegiao;
        //FIll pelo dll
        /*ddl.Items.Add("Porto");
        ddl.Items.Add("Lisboa");
        ddl.Items.Add("Madrid");*/
        //Contactar musicPreferences
        ddl.DataSource = RadioActiveRecord.Regiao.LoadAll();
        ddl.DataBind();
    }

}