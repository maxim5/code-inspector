/// File: VSLabFSICore\viewlets.fs
/// 
/// Author: Antonio Cisternino (cisterni@di.unipi.it)
/// Author: Davide Morelli (morellid@di.unipi.it)
///   
/// ------------------------------------------------------------
/// Copyright (c) Microsoft Corporation.  All rights reserved.
/// 
/// The use and distribution terms for this software are 
/// contained in the file named license.txt, which can be found 
/// in the root of this distribution.
/// By using this software in any fashion, you are agreeing to 
/// be bound by the terms of this license.
///
/// You must not remove this notice, or any other, from this
/// software.
/// ------------------------------------------------------------

/// <summary>
/// The Viewlet class lets you manage the active viewlets, save their docking state and load it back later, close them or hide them.
/// An instance of this class is created by the VSLab startup script (accessible from the F# Interactive Shell with the "Viewlets" name) and should never be created directly.
/// </summary>
module VSLab.Viewlets

open VSLab.Environment
open System.Windows.Forms
open System.Xml
open System.Threading
open VSLab

/// <summary>
/// Initializes the Viewlets class.
/// </summary>
let Init (vspid:int) = 
  _vspid <- vspid
  _viewletsProxy <- new Form(Text="Viewlet Proxy")
  Application.DoEvents()
  //_viewletsProxy.Opacity <- (float 0)
  //Application.DoEvents()
  _viewletsProxy.ShowInTaskbar <- false
  _viewletsProxy.FormBorderStyle <- FormBorderStyle.FixedToolWindow
  _viewletsProxy.Show()
  Application.DoEvents()
  _viewletsProxy.Width <- 1
  _viewletsProxy.Height <- 1
  _viewletsProxy.Top <- -100
  Application.DoEvents()

/// <summary>
/// The number of active viewlets.
/// </summary>
let Count () = _viewletsList.Count

/// <summary>
/// Enumeration of active viewlets.
/// </summary>
let Items () = seq { for v in _viewletsList do yield (v :?> Viewlet) done }
        
/// <summary>
/// Saves the actual viewlets docking state in an xml file.
/// </summary>
/// <param name="filename"> the filename where to store the current viewlets docking state</param>
let Save (filename:string) =
  let xmlDoc = new XmlDocument()
  //Create Parent Node
  let newElem = xmlDoc.CreateElement("VSLab")
  xmlDoc.AppendChild(newElem) |> ignore  
     
  let SerializeViewlet (v:Viewlet) = 
    let newChild = xmlDoc.CreateElement("Viewlet") 
    let childName = xmlDoc.CreateElement("Name")
    childName.InnerText <- v.Name
    newChild.AppendChild(childName) |> ignore 
    let childGuid = xmlDoc.CreateElement("Guid")
    childGuid.InnerText <- v.Guid
    newChild.AppendChild(childGuid) |> ignore 
    let childType = xmlDoc.CreateElement("Type")
    childType.InnerText <- v.GetType().FullName + "," + v.GetType().Assembly.GetName().Name
    newChild.AppendChild(childType) |> ignore 
    newElem.AppendChild(newChild) |> ignore 
      
  Seq.iter (SerializeViewlet) (Items())
  let saveFileDiag = new System.Windows.Forms.SaveFileDialog()
  xmlDoc.Save(filename)
  ()
    
/// <summary>
/// Load the viewlets docking state from an xml file.
/// </summary>
/// <param name="filename"> the filename where to load the viewlets docking state from</param>
let Load (filename:string) =
  let xmlDocRead = new XmlDocument()
  let DeserializeViewlet (child:XmlNode) =
    let childName = child.ChildNodes.Item(0).InnerText
    let childGuid = child.ChildNodes.Item(1).InnerText
    let childTypeName = child.ChildNodes.Item(2).InnerText
    //System.Console.WriteLine("{0} {1} {2}", childName, childGuid, childType)
    let childType = System.Type.GetType(childTypeName)
    if childType = null then
      System.Console.WriteLine("Cannot find Type {0}", childTypeName)
    else
      let viewlet = (box(System.Activator.CreateInstance(childType)) :?> Viewlet)
      viewlet.Name <- childName
      viewlet.Guid <- childGuid
      viewlet.Show()

  xmlDocRead.Load(filename)
  for child in xmlDocRead.FirstChild.ChildNodes do
    DeserializeViewlet child
  ()
  
/// <summary>
/// The list of viewlets actually in the hidden state.
/// </summary>
let HiddenItems () = 
  seq { let l = _viewletsList.ToArray() // Used to avoid unnecessary locks, it can be improved.
    for v in l do let vw = (v :?> Viewlet) in if not vw.ToolWindow.Visible then yield vw done }

/// <summary>
/// Closes all viewlets.
/// </summary>
let Close() =
  // close all viewlets
  Array.iter (fun (v:Viewlet) -> v.Close() |> ignore) (Seq.to_array (Items()))

/// <summary>
/// Closes all viewlets.
/// </summary>
let Quit() =
  Close()
  
/// <summary>
/// Prepares the list of hidden viewlets.
/// </summary>
let PopulateHiddenList () =
  ThreadPool.QueueUserWorkItem(fun _ -> 
    //let appObj = System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE") :?> EnvDTE.DTE
    let appObj = FetchVSDTE _vspid
    let arg = System.String.Join("#__#", Array.of_seq(Seq.map (fun (v:Viewlet) -> v.Name) (HiddenItems())))
    appObj.ExecuteCommand("VSLabAddin+Addin.PopulateHiddenViewletsList", arg)
  ) |> ignore
  
/// <summary>
/// Shows a viewelt whose name is the same as the "name" argument
/// </summary>
/// <param name="name"> the name of the Viewlet to show </param>
let ShowViewlet (name:string) =
  let v = Seq.find (fun (v:Viewlet) -> name = v.Name ) (seq { for v in _viewletsList do let vw = (v :?> Viewlet) in yield vw done })
  v.Show()
  PopulateHiddenList()
    
populateHiddenFun <- PopulateHiddenList
