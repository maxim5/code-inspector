using System;
using System.Collections.Generic;
using System.Linq;

using System.Web.UI.WebControls;

using Microsoft.AspNet.Membership.OpenAuth;

namespace Dev.CasClientWebForm.Account
{
    public partial class Manage : System.Web.UI.Page
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

        protected void Page_Load()
        {
            if (!IsPostBack)
            {
                // 确定要呈现的节
                var hasLocalPassword = OpenAuth.HasLocalPassword(User.Identity.Name);
                setPassword.Visible = !hasLocalPassword;
                changePassword.Visible = hasLocalPassword;

                CanRemoveExternalLogins = hasLocalPassword;

                // 呈现成功消息
                var message = Request.QueryString["m"];
                if (message != null)
                {
                    // 从操作中去除查询字符串
                    Form.Action = ResolveUrl("~/Account/Manage");

                    SuccessMessage =
                        message == "ChangePwdSuccess" ? "已更改你的密码。"
                        : message == "SetPwdSuccess" ? "已设置你的密码。"
                        : message == "RemoveLoginSuccess" ? "已删除外部登录。"
                        : String.Empty;
                    successMessage.Visible = !String.IsNullOrEmpty(SuccessMessage);
                }
            }


            // 对外部帐户列表进行数据绑定
            var accounts = OpenAuth.GetAccountsForUser(User.Identity.Name);
            CanRemoveExternalLogins = CanRemoveExternalLogins || accounts.Count() > 1;
            externalLoginsList.DataSource = accounts;
            externalLoginsList.DataBind();

        }

        protected void setPassword_Click(object sender, EventArgs e)
        {
            if (IsValid)
            {
                var result = OpenAuth.AddLocalPassword(User.Identity.Name, password.Text);
                if (result.IsSuccessful)
                {
                    Response.Redirect("~/Account/Manage?m=SetPwdSuccess");
                }
                else
                {

                    newPasswordMessage.Text = result.ErrorMessage;

                }
            }
        }


        protected void externalLoginsList_ItemDeleting(object sender, ListViewDeleteEventArgs e)
        {
            var providerName = (string)e.Keys["ProviderName"];
            var providerUserId = (string)e.Keys["ProviderUserId"];
            var m = OpenAuth.DeleteAccount(User.Identity.Name, providerName, providerUserId)
                ? "?m=RemoveLoginSuccess"
                : String.Empty;
            Response.Redirect("~/Account/Manage" + m);
        }

        protected T Item<T>() where T : class
        {
            return GetDataItem() as T ?? default(T);
        }


        protected static string ConvertToDisplayDateTime(DateTime? utcDateTime)
        {
            // 你可以更改此方法，以便将 UTC 日期时间转换为所需的显示
            // 偏移值和格式。正在使用当前线程区域设置将它转换成服务器时区，并将其设置为
            // 短日期和长时间字符串的格式。
            return utcDateTime.HasValue ? utcDateTime.Value.ToLocalTime().ToString("G") : "[从不]";
        }
    }
}