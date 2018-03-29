using System;
using System.Collections.Generic;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Text;
using System.Data;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;


namespace ExtAspNet.Examples
{
    public class PageBase : System.Web.UI.Page
    {
        protected override void OnInit(EventArgs e)
        {
            base.OnInit(e);

            if (!IsPostBack)
            {
                if (PageManager.Instance != null)
                {
                    HttpCookie themeCookie = Request.Cookies["Theme.EN"];
                    if (themeCookie != null)
                    {
                        string themeValue = themeCookie.Value;

                        if (IsSystemTheme(themeValue))
                        {
                            PageManager.Instance.Theme = (Theme)Enum.Parse(typeof(Theme), themeValue, true);
                        }
                        else
                        {
                            PageManager.Instance.CustomTheme = themeValue;
                        }
                    }

                    HttpCookie langCookie = Request.Cookies["Language.EN"];
                    if (langCookie != null)
                    {
                        string langValue = langCookie.Value;
                        PageManager.Instance.Language = (Language)Enum.Parse(typeof(Language), langValue, true);
                    }
                }
            }

        }

        private bool IsSystemTheme(string themeName)
        {
            string[] themes = Enum.GetNames(typeof(Theme));
            foreach (string theme in themes)
            {
                if (theme.ToLower() == themeName)
                {
                    return true;
                }
            }
            return false;
        }

        protected readonly static JArray SHENG_JSON = JArray.Parse("[\"Beijing\"]");
        protected readonly static JObject SHI_JSON = JObject.Parse("{\"Beijing\":[\"Beijing City\"]}");
        protected readonly static JObject XIAN_JSON = JObject.Parse("{\"Beijing City\":[\"Dongcheng Area\",\"Xicheng Area\",\"Congwen Area\",\"Xuanwu Area\",\"Chaoyang Area\",\"Fengtai Area\",\"Shijingshan Area\",\"Haidian Area\",\"Mentougou Area\",\"Fangshan Area\",\"Tongzhou Area\",\"Shunyi Area\",\"Changping Area\",\"Daxing Area\",\"Huairou Area\",\"Pinggu Area\",\"Miyun County\",\"Yanqing County\"]}");


        #region Grid Related

        /// <summary>
        /// Selected rows
        /// </summary>
        /// <param name="grid"></param>
        /// <returns></returns>
        protected string HowManyRowsAreSelected(Grid grid)
        {
            StringBuilder sb = new StringBuilder();
            int selectedCount = grid.SelectedRowIndexArray.Length;
            if (selectedCount > 0)
            {
                sb.AppendFormat("Totally select {0} rows: ", selectedCount);
                sb.Append("<table style=\"width:500px;\">");

                sb.Append("<tr><th>Row Number</th>");
                foreach (string datakey in grid.DataKeyNames)
                {
                    sb.AppendFormat("<th>{0}</th>", datakey);
                }
                sb.Append("</tr>");
                 

                for (int i = 0; i < selectedCount; i++)
                {
                    int rowIndex = grid.SelectedRowIndexArray[i];
                    sb.Append("<tr>");

                    sb.AppendFormat("<td>{0}</td>", rowIndex + 1);


                    if (grid.AllowPaging && !grid.IsDatabasePaging)
                    {
                        rowIndex = grid.PageIndex * grid.PageSize + rowIndex;
                    }

                    object[] dataKeys = grid.DataKeys[rowIndex];
                    for (int j = 0; j < dataKeys.Length; j++)
                    {
                        sb.AppendFormat("<td>{0}</td>", dataKeys[j]);
                    }

                    sb.Append("</tr>");
                }
                sb.Append("</table>");
            }
            else
            {
                sb.Append("<strong>No selection!</strong>");
            }

            return sb.ToString();
        }

        /// <summary>
        /// Get gender's literal value
        /// </summary>
        /// <param name="gender"></param>
        /// <returns></returns>
        protected string GetGender(object gender)
        {
            if (Convert.ToInt32(gender) == 1)
            {
                return "Man";
            }
            else
            {
                return "Woman";
            }
        }

        protected DataTable GetClassDataTable()
        {
            DataTable table = new DataTable();
            table.Columns.Add(new DataColumn("Id", typeof(int)));
            table.Columns.Add(new DataColumn("Name", typeof(String)));
            table.Columns.Add(new DataColumn("EntranceYear", typeof(String)));
            table.Columns.Add(new DataColumn("LogTime", typeof(DateTime)));
            table.Columns.Add(new DataColumn("Desc", typeof(string)));

            DataRow row = table.NewRow();

            row[0] = 101;
            row[1] = "Class 1";
            row[2] = "2000";
            row[3] = DateTime.Parse("2000-09-01");
            row[4] = "Class 1 is created in 1/9/2000, class monitor Fei Hu, teacher in charge Mr Zhou.";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 102;
            row[1] = "Class 2";
            row[2] = "2005";
            row[3] = DateTime.Parse("2005-09-01");
            row[4] = "Class 1 is created in 1/9/2000, class monitor Tingting Dong, teacher in charge Mr Zheng.";
            table.Rows.Add(row);

            return table;
        }


        /// <summary>
        /// Get datatable source
        /// </summary>
        /// <returns></returns>
        protected DataTable GetDataTable()
        {
            DataTable table = new DataTable();
            table.Columns.Add(new DataColumn("Id", typeof(int)));
            table.Columns.Add(new DataColumn("Name", typeof(String)));
            table.Columns.Add(new DataColumn("EntranceYear", typeof(String)));
            table.Columns.Add(new DataColumn("AtSchool", typeof(bool)));
            table.Columns.Add(new DataColumn("Major", typeof(String)));
            table.Columns.Add(new DataColumn("Group", typeof(int)));
            table.Columns.Add(new DataColumn("Gender", typeof(int)));
            table.Columns.Add(new DataColumn("LogTime", typeof(DateTime)));
            table.Columns.Add(new DataColumn("Desc", typeof(string)));
            table.Columns.Add(new DataColumn("Guid", typeof(Guid)));
            table.Columns.Add(new DataColumn("Hobby", typeof(String)));
            // Hobby：reading,basketball,travel,movie,music

            DataRow row = table.NewRow();

            row[0] = 101;
            row[1] = "Pingping Chen";
            row[2] = "2000";
            row[3] = true;
            row[4] = "Department of Computer Science";
            row[5] = 1;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-100);
            row[8] = "Pingping Chen, woman, 20 years old, born in a small mountain village in the south of China, graduated from the University of Science and Technology of China.";
            row[9] = new Guid();
            row[10] = "reading,basketball,travel";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 102;
            row[1] = "Fei Hu";
            row[2] = "2008";
            row[3] = false;
            row[4] = "Department of Chemistry";
            row[5] = 1;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-90);
            row[8] = "Fei Hu, man, 20 years old, born in a small mountain village in the north of China, graduated from the University of Science and Technology of Nanfang.";
            row[9] = new Guid();
            row[10] = "reading,basketball";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 103;
            row[1] = "Tingting Jin";
            row[2] = "2001";
            row[3] = true;
            row[4] = "Department of Accounting";
            row[5] = 2;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-80);
            row[8] = "Tingting Jin, woman, 28 years old, born in a small mountain village in Hainan island  of China, graduated from the University of Science and Technology of China.";
            row[9] = new Guid();
            row[10] = "reading,basketball,music";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 104;
            row[1] = "Guo Pan";
            row[2] = "2008";
            row[3] = false;
            row[4] = "Department of International Economy";
            row[5] = 2;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-70);
            row[8] = "Guo Pan, man, 22 years old, born in a small mountain village in Macau of China, graduated from the University of Science and Technology of China.";
            row[9] = new Guid();
            row[10] = "reading,music";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 105;
            row[1] = "Yingying Wu";
            row[2] = "2002";
            row[3] = true;
            row[4] = "Department of Marketing Management";
            row[5] = 3;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-60);
            row[8] = "Yingying Wu, woman, 26 years old, born in a small mountain village in Fujian of China, graduated from the University of Science and Technology of Hongkong.";
            row[9] = new Guid();
            row[10] = "reading,movie,music";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 106;
            row[1] = "Bo Zhang";
            row[2] = "2003";
            row[3] = false;
            row[4] = "Department of Financial Management";
            row[5] = 3;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-50);
            row[8] = "Bo Zhang, man, 28 years old, born in a small mountain village in Zhejiang of China, graduated from the University of Science and Technology of China.";
            row[9] = new Guid();
            row[10] = "movie,music";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 107;
            row[1] = "Qianqian Yang";
            row[2] = "2000";
            row[3] = true;
            row[4] = "Department of Chemistry";
            row[5] = 4;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-40);
            row[8] = "Qianqian Yang, woman, 25 years old, born in a small mountain village in the north of China, graduated from the University of Science and Technology of Beijing.";
            row[9] = new Guid();
            row[10] = "travel,movie,music";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 108;
            row[1] = "Chao Dong";
            row[2] = "2004";
            row[3] = false;
            row[4] = "Department of Chemistry";
            row[5] = 4;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-30);
            row[8] = "Chao Dong, man, 26 years old, born in a small mountain village in Henan of China, graduated from the University of Science and Technology of China.";
            row[9] = new Guid();
            row[10] = "basketball,movie,music";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 109;
            row[1] = "Juanjuan Zhang";
            row[2] = "2003";
            row[3] = true;
            row[4] = "Department of Physics";
            row[5] = 5;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-20);
            row[8] = "Juanjuan Zhang, woman, 25 years old, born in a small mountain village in Guangxi of China, graduated from the University of Science and Technology of Nanfang.";
            row[9] = new Guid();
            row[10] = "reading,travel,movie,music";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 110;
            row[1] = "Peng Ye";
            row[2] = "2006";
            row[3] = false;
            row[4] = "Department of Electronic Commerce";
            row[5] = 5;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-10);
            row[8] = "Peng Ye, man, 23 years old, born in a small mountain village in Anhui of China, graduated from the University of Science and Technology of Guofang.";
            row[9] = new Guid();
            row[10] = "reading,movie,music";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 111;
            row[1] = "Lingling Li";
            row[2] = "2002";
            row[3] = true;
            row[4] = "Department of Management";
            row[5] = 5;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-5);
            row[8] = "Lingling Li, woman, 22 years old, born in a small mountain village in Taiwan of China, graduated from the University of Science and Technology of Taiwan.";
            row[9] = new Guid();
            row[10] = "reading,travel,music";
            table.Rows.Add(row);


            return table;
        }

        /// <summary>
        /// Get datatable source 2
        /// </summary>
        /// <returns></returns>
        protected DataTable GetDataTable2()
        {
            DataTable table = new DataTable();
            table.Columns.Add(new DataColumn("Id", typeof(int)));
            table.Columns.Add(new DataColumn("Name", typeof(String)));
            table.Columns.Add(new DataColumn("EntranceYear", typeof(String)));
            table.Columns.Add(new DataColumn("AtSchool", typeof(bool)));
            table.Columns.Add(new DataColumn("Major", typeof(String)));
            table.Columns.Add(new DataColumn("Group", typeof(int)));
            table.Columns.Add(new DataColumn("Gender", typeof(int)));
            table.Columns.Add(new DataColumn("LogTime", typeof(DateTime)));
            table.Columns.Add(new DataColumn("Desc", typeof(string)));

            DataRow row = table.NewRow();
            row[0] = 101;
            row[1] = "Pingping Zhang";
            row[2] = "2000";
            row[3] = true;
            row[4] = "Department of Materials Science and Engineering";
            row[5] = 1;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-100);
            row[8] = "Pingping Zhang, woman, 20 years old, born in a small mountain village in the south of China, graduated from the University of Science and Technology of China.";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 102;
            row[1] = "Fei Chen";
            row[2] = "2001";
            row[3] = false;
            row[4] = "Department of Physics";
            row[5] = 1;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-90);
            row[8] = "Fei Chen, man, 20 years old, born in a small mountain village in the north of China, graduated from the University of Science and Technology of Nanfang.";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 103;
            row[1] = "Tingting Dong";
            row[2] = "2008";
            row[3] = true;
            row[4] = "Department of Chemistry";
            row[5] = 2;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-80);
            row[8] = "Tingting Dong, woman, 28 years old, born in a small mountain village in Hainan island  of China, graduated from the University of Science and Technology of China.";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 104;
            row[1] = "Guo Liu";
            row[2] = "2002";
            row[3] = false;
            row[4] = "Department of Chemistry";
            row[5] = 2;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-70);
            row[8] = "Guo Liu, man, 22 years old, born in a small mountain village in Macau of China, graduated from the University of Science and Technology of China.";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 105;
            row[1] = "Yingying Kang";
            row[2] = "2008";
            row[3] = true;
            row[4] = "Department of Mathematics";
            row[5] = 3;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-60);
            row[8] = "Yingying Kang, woman, 26 years old, born in a small mountain village in Fujian of China, graduated from the University of Science and Technology of Hongkong.";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 106;
            row[1] = "Bo Peng";
            row[2] = "2003";
            row[3] = false;
            row[4] = "Department of Mathematics";
            row[5] = 3;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-50);
            row[8] = "Bo Peng, man, 28 years old, born in a small mountain village in Zhejiang of China, graduated from the University of Science and Technology of China.";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 107;
            row[1] = "Qianqian Huang";
            row[2] = "2000";
            row[3] = true;
            row[4] = "Department of Physics";
            row[5] = 4;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-40);
            row[8] = "Qianqian Huang, woman, 25 years old, born in a small mountain village in the north of China, graduated from the University of Science and Technology of Beijing.";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 108;
            row[1] = "Chao Tang";
            row[2] = "2004";
            row[3] = false;
            row[4] = "Department of Modern Physics";
            row[5] = 4;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-30);
            row[8] = "Chao Tang, man, 26 years old, born in a small mountain village in Henan of China, graduated from the University of Science and Technology of China.";
            table.Rows.Add(row);


            row = table.NewRow();
            row[0] = 109;
            row[1] = "Juanjuan Yang";
            row[2] = "2003";
            row[3] = true;
            row[4] = "Department of Astronomy";
            row[5] = 5;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-20);
            row[8] = "Juanjuan Yang, woman, 25 years old, born in a small mountain village in Guangxi of China, graduated from the University of Science and Technology of Nanfang.";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 110;
            row[1] = "Peng Xu";
            row[2] = "2002";
            row[3] = false;
            row[4] = "Department of Astronomy";
            row[5] = 5;
            row[6] = 1;
            row[7] = DateTime.Now.AddDays(-10);
            row[8] = "Peng Xu, man, 23 years old, born in a small mountain village in Anhui of China, graduated from the University of Science and Technology of Guofang.";
            table.Rows.Add(row);

            row = table.NewRow();
            row[0] = 111;
            row[1] = "Lingling Ji";
            row[2] = "2006";
            row[3] = true;
            row[4] = "Department of Automation";
            row[5] = 5;
            row[6] = 0;
            row[7] = DateTime.Now.AddDays(-5);
            row[8] = "Lingling Ji, woman, 22 years old, born in a small mountain village in Taiwan of China, graduated from the University of Science and Technology of Taiwan.";
            table.Rows.Add(row);


            return table;
        }

        /// <summary>
        /// Get empty datatable
        /// </summary>
        /// <returns></returns>
        protected DataTable GetEmptyDataTable()
        {
            DataTable table = new DataTable();
            table.Columns.Add(new DataColumn("Id", typeof(int)));
            table.Columns.Add(new DataColumn("Name", typeof(String)));
            table.Columns.Add(new DataColumn("EntranceYear", typeof(String)));
            table.Columns.Add(new DataColumn("AtSchool", typeof(bool)));
            table.Columns.Add(new DataColumn("Major", typeof(String)));
            table.Columns.Add(new DataColumn("Group", typeof(int)));
            table.Columns.Add(new DataColumn("Gender", typeof(int)));


            return table;
        }


        #endregion
    }

}
