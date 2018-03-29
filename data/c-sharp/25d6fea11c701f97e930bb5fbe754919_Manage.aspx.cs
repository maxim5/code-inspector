using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

public partial class Manage : System.Web.UI.Page
{
    static protected List<Sensor> deviceSensors;
    protected override void OnInit(EventArgs e)
    {
        DropDownList_DisplayType.DataSource = PersistanceLayer.GetDisplays();
        DropDownList_DisplayType.DataBind();

        DDL_DeviceSelect.DataSource = PersistanceLayer.GetDevices();
        DDL_DeviceSelect.DataBind();
        base.OnInit(e);
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        if (!IsPostBack)
        {
            TBX_DeviceDescription.Text = "";
            TB_DeviceName.Text = "";
            LBX_DeviceSensors.Items.Clear();

            if (!string.IsNullOrEmpty(Request.QueryString["device"]))
            {
                deviceSensors = PersistanceLayer.GetSensors(Request.QueryString["device"]);

                Device deviceInfo = PersistanceLayer.GetDeviceInfo(Request.QueryString["device"]);

                if (!string.IsNullOrEmpty(deviceInfo.deviceName))
                {
                    TBX_DeviceDescription.Text = deviceInfo.description;
                    TB_DeviceName.Text = deviceInfo.deviceName;

                    foreach (Sensor sn in deviceSensors)
                    {
                        LBX_DeviceSensors.Items.Add(sn.sensorName);
                    }
                }
            }
            else
            {
            }
        }
    }

    protected void DDL_DeviceSelect_SelectedIndexChanged(object sender, EventArgs e)
    {
        Response.Redirect("Manage.aspx?device=" + DDL_DeviceSelect.SelectedItem.Text);
    }

    protected void LBX_DeviceSensors_SelectedIndexChanged(object sender, EventArgs e)
    {
        Sensor temp = deviceSensors.Find( delegate(Sensor t) { return t.sensorName == LBX_DeviceSensors.SelectedItem.ToString(); } );

        //TODO: BUG here with sensor display types etc or in PersistanceLayer.cs
        TB_DisplayType.Text = PersistanceLayer.GetDisplayInfo(temp.displayType);

        foreach (ListItem mItem in DropDownList_DisplayType.Items)
        {
            mItem.Selected = false;
            if (mItem.Text == temp.displayType)
                mItem.Selected = true;
        }

        if (temp != null)
        {
            TBX_SensorDescription.Text = temp.descript;
        }
    }
}
