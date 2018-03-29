#light
namespace LandingSite.Controllers
open System
open System.Transactions
open System.Web.Security
open System.Web.Mvc
open LandingSite.Models
open WebMatrix.WebData
open Microsoft.Web.WebPages.OAuth

type ManageMessageId =
        | ChangePasswordSuccess =1
        | SetPasswordSucess = 2
        | RemoveLoginSuccess=3
//[<InitializeSimpleMembership>]
type AccountController()=
    inherit Controller()
    //let apply f x y = f(x,y)
    let (?<-) (viewData:ViewDataDictionary) (name:string) (value:'T) =
        viewData.Add(name, box value)
    let (=>) a b = a, box b
    let ToRv dic = System.Web.Routing.RouteValueDictionary(dic)
    
    [<AllowAnonymous>]
    member this.Login(returnUrl:string) =
        //apply this.ViewData.Add "ReturnUrl" returnUrl
        this.ViewData?ReturnUrl<-returnUrl
        this.View()
    
    [<HttpPost>]
    [<AllowAnonymous>]
    [<ValidateAntiForgeryToken>]
    member this.Login(model:LoginModel,returnUrl) :ActionResult = 
        if this.ModelState.IsValid && WebSecurity.Login(model.UserName,model.Password,persistCookie= model.RememberMe) then 
            this.RedirectToLocal(returnUrl)
        else
            this.ModelState.AddModelError(String.Empty,"The user name or password provided is incorrect.")
            this.View(model) :>ActionResult

    [<HttpPost>]
    [<ValidateAntiForgeryToken>]
    member this.LogOff() :ActionResult =
        WebSecurity.Logout()
        this.RedirectToAction("Index","Home") :>ActionResult

    [<AllowAnonymous>]
    member this.Register() = 
        this.View() 
    

    [<HttpPost>]
    [<AllowAnonymous>]
    member this.Register(model:RegisterModel) : ActionResult =
        let mutable result:ActionResult = null
        if this.ModelState.IsValid then
            try
                WebSecurity.CreateUserAndAccount(model.UserName,model.Password) |> ignore
                WebSecurity.Login(model.UserName,model.Password) |> ignore
                result <- this.RedirectToAction("Index","Home")
            with | :? System.Web.Security.MembershipCreateUserException as e ->
                let ec:string=AccountController.ErrorCodeToString(e.StatusCode)
                this.ModelState.AddModelError(String.Empty,ec)
                result <- this.View(model)
        result
     
    [<HttpPost>]
    [<ValidateAntiForgeryToken>]
    member this.Disassociate(provider,providerUserId) :ActionResult =
        let ownerAccount=OAuthWebSecurity.GetUserName(provider, providerUserId)
        let mutable message:ManageMessageId= ManageMessageId.ChangePasswordSuccess
        if ownerAccount = this.User.Identity.Name then
            use scope = new TransactionScope(TransactionScopeOption.Required, new TransactionOptions(IsolationLevel = IsolationLevel.Serializable))
            let hasLocalAccount = OAuthWebSecurity.HasLocalAccount(WebSecurity.GetUserId(this.User.Identity.Name))
            if hasLocalAccount || OAuthWebSecurity.GetAccountsFromUserName(this.User.Identity.Name).Count >1 then
                OAuthWebSecurity.DeleteAccount(provider, providerUserId) |> ignore
                scope.Complete()
                message <- ManageMessageId.RemoveLoginSuccess
        this.RedirectToAction("Manage",ToRv <| dict  [ "Message" => message]) :> ActionResult
    member this.Manage(?message:ManageMessageId) :ActionResult =
        
        this.ViewData?StatusMessage <- 
            match message with 
            | Some ManageMessageId.ChangePasswordSuccess -> "Your password has been changed." 
            | Some ManageMessageId.SetPasswordSucess -> "Your password has been set."
            | Some ManageMessageId.RemoveLoginSuccess -> "The external login was removed."
            | None -> String.Empty
        this.ViewData?HasLocalPassword <- OAuthWebSecurity.HasLocalAccount(WebSecurity.GetUserId(this.User.Identity.Name))
        this.ViewData?ReturnUrl <- this.Url.Action("Manage")
        this.View() :> ActionResult
//helpers
    
    member m.RedirectToLocal(returnUrl) : ActionResult =
        if m.Url.IsLocalUrl(returnUrl)  then
            m.Redirect(returnUrl) :>ActionResult
        else
            m.RedirectToAction( "Index", "Home") :>ActionResult
   
    static member ErrorCodeToString(createStatus:MembershipCreateStatus) =
        match createStatus with 
        | MembershipCreateStatus.DuplicateUserName -> "User name already exists. Please enter a different user name."
        | MembershipCreateStatus.DuplicateEmail -> "A user name for that e-mail address already exists. Please enter a different e-mail address."
        | MembershipCreateStatus.InvalidPassword -> "The password provided is invalid. Please enter a valid password value."
        | MembershipCreateStatus.InvalidEmail -> "The e-mail address provided is invalid. Please check the value and try again."
        | MembershipCreateStatus.InvalidAnswer -> "The password retrieval answer provided is invalid. Please check the value and try again."
        | MembershipCreateStatus.InvalidQuestion ->"The password retrieval question provided is invalid. Please check the value and try again."
        | MembershipCreateStatus.InvalidUserName -> "The user name provided is invalid. Please check the value and try again."
        | MembershipCreateStatus.ProviderError -> "The authentication provider returned an error. Please verify your entry and try again. If the problem persists, please contact your system administrator."
        | MembershipCreateStatus.UserRejected -> "The user creation request has been canceled. Please verify your entry and try again. If the problem persists, please contact your system administrator."
        | _ -> "An unknown error occurred. Please verify your entry and try again. If the problem persists, please contact your system administrator."
    