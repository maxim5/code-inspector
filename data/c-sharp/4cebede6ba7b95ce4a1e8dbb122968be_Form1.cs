
﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using WindowsFormsApplication1.Database_Objects;
using WindowsFormsApplication1.Steven_s;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        
        string[] inputtext = new string[10];

        public static int maxlines = 0; //indexical, for opening .frm files

        //Greg's Variables
        Graphics g;
        LineDrawer w = new LineDrawer();
        bool candrawline = false;
        bool scaleSelecting = false;
        Keys key;
        int desiredLength;
        float zoom = 1;
        int DownX;
        int DownY;
        double profsx;//percent of screen x
        double profsy;//percent of screen y
        double drawspace_drawspaceContainer_width_diff;
        double drawspace_drawspaceContainer_height_diff; 
        public DataSet Data;
        DatabaseData Db;
        MouseEventArgs M;
        //**
        Color bgcolor;
        String BackgroundFile;
        CreateBoundary createBoundaryForm;
        SelectCrop SelectCropForm;
        BlockGroup newBlockGroup;
        SetScale ScaleSelectionForm;

        public Form1()
        {
            this.Icon = new System.Drawing.Icon("tractor_icon.ico");
            bgcolor = new Color();
            bgcolor = System.Drawing.Color.Black;
            clearinput(ref inputtext);
            
            InitializeComponent();
            drawspaceContainer.Width = (this.Width - 15);//-15 is just in case for vertical slider
            
            drawspace.Width = drawspaceContainer.Width - 15;
            drawspace.Height = drawspaceContainer.Height - 15;
            BackgroundFile = "0";

            //GFX Create
            //g = CreateGraphics();
            w.setLineColor(Color.Red);
            this.Cursor = Cursors.Default;
            Splash S = new Splash();
            S.Show();
        }

        //console checking and events
        public void CheckTheKeys(object who, KeyPressEventArgs k)
        {

            if (k.KeyChar == (char)13)
            {
                string preinput = console.Text + " ";
                for (int i = 0, z = 0, min = 0; i < preinput.Length; i++)
                {
                    if (preinput[i] == (char)32)
                    {
                        //check if consecutive spaces
                        if (min == i)
                        {
                            min++;
                            continue;
                        }
                        //valid separator detected
                        for (int j = min; j < i; j++)
                        {
                            inputtext[z] += preinput[j];
                        }
                        min = i + 1;
                        z++;
                    }
                }
                if (inputtext[0] == "exit")
                {
                    if (inputtext[1] == "y")
                        Application.Exit();
                    DialogResult exitresult = MessageBox.Show("Are you sure you want to exit?", "Exit", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation);
                    if (exitresult == DialogResult.Yes)
                        Application.Exit();
                }
                //requries 2 arguments
                else if (inputtext[0] == "line")
                {
                    setupline();
                }
                /*else if (inputtext[0] == "save")
                {
                    sendoutput("Save functionality not yet available!");
                }*/
                else
                    sendoutput("Command '" + console.Text + "' not recongnized.");
                console.Text = "";
                clearinput(ref inputtext);
            }
        }
        protected void setupline()
        {
            if (inputtext[1] == "" || inputtext[2] == "" || inputtext[3] == "" || inputtext[4] == "")
            {
                LineGoOrNo();
            }
            else
            {
                for (int i = 1; i <= 4; i++)
                {
                    for (int j = 0; j < inputtext[i].Length; j++)
                    {
                        if ((inputtext[i])[j] == (char)45)
                            continue;
                        if ((int)char.GetNumericValue((inputtext[i])[j]) == -1)
                        {
                            sendoutput("Invaild argument(s) for command '" + inputtext[0] + "'.");
                            return;
                        }
                    }
                }
                double x1, y1, x2, y2 = 0;
                x1 = double.Parse(inputtext[1]);
                y1 = double.Parse(inputtext[2]);
                x2 = double.Parse(inputtext[3]);
                y2 = double.Parse(inputtext[4]);
                Line p = new Line(x1, y1, 0, x2, y2, 0);
                w.DrawerAddLine(p);
                sendoutput("Line from (" + inputtext[1] + "," + inputtext[2] + ") to (" + inputtext[3] + "," + inputtext[4] + ") drawn.");
            }
        }
        public void sendoutput(string x)
        {
            output.Text += x + "\r\n";
            output.SelectionStart = output.TextLength;
            output.ScrollToCaret();

        }
        //Menu Click Events
        protected void NewClick(object who, EventArgs e)
        {
            w.d.ClearDrawnLines();
            output.Clear();
            drawspace.Refresh();
        }
        protected void SaveClick(object who, EventArgs e)
        {
            Save savey = new Save();
            if (BackgroundFile == null || Db==null)
            {
                savey.SaveIt("save.frm", w.d.DrawnLinesList);
            }
            else
            {
                savey.SaveIt("save.frm", w.d.DrawnLinesList, BackgroundFile, Db.Connection.filepath);
            }
        }
        protected void OpenClick(object who, EventArgs e)
        {
            NewClick(who, e);
            BinaryFormatter bFormatter = new BinaryFormatter();
            StreamReader read = new StreamReader("linecount.flc");
            string readmax = read.ReadLine();
            string isBGImage = read.ReadLine();
            string isDBPath = read.ReadLine();
            read.Close();
            Stream s = File.Open("save.frm", FileMode.Open);
            maxlines = int.Parse(readmax);
            for (int j = 0; j <= maxlines; j++)
            {
                w.d.DrawnLinesList.Add((Line)bFormatter.Deserialize(s)); //just assigning to make it happy
            }
            if (bool.Parse(isBGImage))
            {
                BackgroundFile = (string)bFormatter.Deserialize(s);
                drawspace.BackgroundImageLayout = ImageLayout.Stretch;
                drawspace.BackgroundImage = new Bitmap(BackgroundFile);
                drawspace.Size = drawspace.BackgroundImage.Size;
            }
            if (bool.Parse(isDBPath))
            {
                if (Db == null)
                {
                    Db = new DatabaseData((string)bFormatter.Deserialize(s));
                }
                else
                {
                    Db.Connection.filepath = (string)bFormatter.Deserialize(s);
                }
                Db.Connection.SetConnection();
            }
            Line.onLine = 0;
            s.Close();
            drawspace.Refresh();
        }
        protected void ExitClick(object who, EventArgs e)
        {
            DialogResult exitresult = MessageBox.Show("Are you sure you want to exit?", "Exit", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation);
            if (exitresult == DialogResult.Yes)
                Application.Exit();
        }
        protected void AboutClick(object who, EventArgs e)
        {
            MessageBox.Show("All your farm are belong to us.\n\nCreated by:\n\nCorey Dihel\nGregory Dzhezyan\nMatthew Johnston\nSteven Sanouvong\nJeremiah Tookey", "About", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }
        protected void LineButtonClick(object who, EventArgs e)
        {
            if (toolStripButton7.Enabled)
            {
                toolStripButton7.Enabled = false;
                scaleSelectButton.Enabled = false;
            }
            else
            {
                toolStripButton7.Enabled = true;
                scaleSelectButton.Enabled = true;
            }
            LineGoOrNo();
        }
        protected void LineGoOrNo()
        {
            if (candrawline == false)
            {
                candrawline = true;
                this.Cursor = Cursors.Cross;
                this.toolStripButton1.Checked = true;
            }
            else
            {
                candrawline = false;
                this.Cursor = Cursors.Default;
                this.toolStripButton1.Checked = false;
            }
        }
        protected void SelectButtonClick(object who, EventArgs e)
        {
            if (toolStripButton1.Enabled)
            {
                toolStripButton1.Enabled = false;
                scaleSelectButton.Enabled = false;
            }
            else
            {
                toolStripButton1.Enabled = true;
                scaleSelectButton.Enabled = true;
            }
        }
        //OptionsClick
        //This is the event handler for clicking on the Options item under the tools menu
        //It will change basic options such as the background image, Database Filepath
        //Woot!! Jeremy was here!!
        protected void OptionsClick(object who, EventArgs e)
        {
            OptionsForm OpF;
            if (Db == null)
            {
                OpF = new OptionsForm(bgcolor, BackgroundFile);
            }
            else
            {
                OpF = new OptionsForm(bgcolor, BackgroundFile, Db.Connection.filepath);
            }
            if (OpF.ShowDialog() == DialogResult.OK)
            {
                if (!OpF.isImage)
                {
                    drawspace.BackgroundImage = null;
                    bgcolor = OpF.BGColor;
                    drawspace.BackColor = bgcolor;
                }
                else
                {
                    drawspace.BackgroundImageLayout = ImageLayout.Stretch;
                    if (OpF.imageFilePath.Contains(".bmp"))
                    {
                        BackgroundFile = OpF.imageFilePath;
                        //drawspace.Size = new Bitmap(BackgroundFile);
                        drawspace.BackgroundImage = new Bitmap(BackgroundFile);
                        drawspace.Size = drawspace.BackgroundImage.Size;
                        zoom = 1;
                    }
                }
                if (OpF.isDataBase && Db == null)
                {
                    Db = new DatabaseData(OpF.dbFilePath);
                    if (OpF.F2.isPassword)
                    {
                        Db.Connection.Passwrd = OpF.F2.Password;
                    }
                    Db.Connection.SetConnection();
                }
            }
            OpF.Dispose();
            //MessageBox.Show("Data base is made? FilePath = " + Db.Connection.filepath);
        }
        protected void clearinput(ref string[] x)
        {
            for(int i = 0; i < x.Length; i++)
            {
                x[i] = "";
            }
        }
        //Greg Functions
        private void Form1_Resize(object sender, EventArgs e)
        {
            drawspaceContainer.Width = (this.Width - 15);//-15 is just in case for vertical slider
            drawspace.Width = (this.Width-20);
            drawspaceContainer.Height = this.Height * 2 / 3;
            drawspace.Height = drawspaceContainer.Height - 5;
            //console.Location= new Point(0, (this.Height - console.Height - 60));
            console.Width = this.Width-15;
            output.Location = new Point(0, (drawspaceContainer.Location.X + drawspaceContainer.Height + 30));
            output.Height = console.Location.Y - output.Location.Y;
            output.Width = this.Width-15;
            drawspace.Refresh();
        }
        private void drawspace_Paint(object sender, PaintEventArgs e)
        {
            g = e.Graphics;
            
            if (zoom <= 0) zoom = 1;
            g.ScaleTransform(zoom, zoom);
            w.DrawerShowLines(g);
            g.DrawString(zoom.ToString(), new Font(FontFamily.GenericSerif, 10), Brushes.Blue, (drawspace.Width/2)/zoom, (drawspace.Height/2)/zoom);
           
            //makedrawspace(g);
        }
        private void drawspace_MouseMove(object sender, MouseEventArgs e)
        {
            
            if (Control.ModifierKeys == Keys.Shift) key = Keys.Shift;
            else key = 0;
            profsx = ((double)(e.X) / drawspace.Width);
            profsy = ((double)(e.Y) / drawspace.Height);
            w.zoom(zoom);
            using (Graphics g2 = drawspace.CreateGraphics())
            {
                g2.ScaleTransform(zoom, zoom);
                w.DrawerOnMouseMove(e, g2, candrawline, key);
            }
            if (e.Button == MouseButtons.Middle)
            {
                drawspace.Location = new Point(drawspace.Left + (e.X - DownX), drawspace.Top + (e.Y - DownY));
            }
            if ((candrawline && w.HasBegunDrawing) || w.getCurrentSelcetedLine().isLineBeingDraged==true ||w.getCurrentSelcetedLine().isPointBeingDraged==true)
            {
                drawspace.Refresh();
            }
            if (zoom > 1 && e.Delta == 0) drawspace.Invalidate(drawspaceContainer.DisplayRectangle);
            M = e;
        }
        private void drawspace_MouseDown(object sender, MouseEventArgs e)
        {
            drawspace.Focus();
            if (e.Button == MouseButtons.Right && w.getCurrentSelcetedLine().canMouseGrabLine(e) == true) w.LineContextMenu.Show(drawspace, e.Location);
            if (createBoundaryForm != null && createBoundaryForm.isEnteringTubingDirection)
            {
                if (w.HasBegunDrawing)
                {
                    createBoundaryForm.tubingCoord1 = w.p.pt1;
                    createBoundaryForm.tubingCoord2 = w.p.pt2;
                    createBoundaryForm.Show();
                    createBoundaryForm.isEnteringTubingDirection = false;
                    LineGoOrNo();
                    scaleSelectButton.Enabled = true;
                    toolStripButton7.Enabled = true;
                }
                w.DrawerOnMouseDown(e, candrawline, desiredLength, true);
                
            }
            else if (ScaleSelectionForm != null && scaleSelecting)
            {
                if (w.HasBegunDrawing)
                {
                    ScaleSelectionForm.refLine = w.p;
                    ScaleSelectionForm.ShowDialog();
                    scaleSelecting = false;
                }
                w.DrawerOnMouseDown(e, candrawline, desiredLength, true);
            }
            else
            {
                if (w.HasBegunDrawing)
                {
                    sendoutput("Line from (" + Convert.ToString(w.p.pt1.getX()) + "," + Convert.ToString(w.p.pt1.getY()) + ") to (" + Convert.ToString(w.p.pt2.getX()) + "," + Convert.ToString(w.p.pt2.getY()) + ") drawn.");
                }
                w.DrawerOnMouseDown(e, candrawline, desiredLength);
                DownX = e.X;
                DownY = e.Y;
                drawspace.Refresh();
            }

        }
        private void drawspace_MouseUp(object sender, MouseEventArgs e)
        {
            if (createBoundaryForm != null && createBoundaryForm.isEnteringTubingDirection)
            {
                w.DrawerOnMouseUp(e);
                desiredLength = 0;
                drawspace.Refresh();
            }
            else if (ScaleSelectionForm != null && scaleSelecting)
            {
                w.DrawerOnMouseUp(e);
                desiredLength = 0;
                drawspace.Refresh();
            }
            else
            {
                w.DrawerOnMouseUp(e);
                desiredLength = 0;
                drawspace.Refresh();
            }
        }
        private void drawspace_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Delete)
            {
                w.DrawerDeleteLine();
            }
            else if (e.KeyCode == Keys.Back)
            {
                w.DrawerDeleteLine();
            }
            else if (Control.ModifierKeys == Keys.Control && e.KeyCode == Keys.D)
            {
                w.DrawerCopyLine();
            }
            else if (e.KeyCode == Keys.M)
            {
                if (w.isMultiLineSelectEnabled == false)
                {
                    w.DrawerEnableMultiSelect(true);
                }
                else w.DrawerEnableMultiSelect(false);
            }
            if (e.KeyCode == Keys.T)
            {
               
            }

            drawspace.Refresh();
        }
        private void drawspace_MouseWheel(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            if (e.Delta > 0 && zoom != 4)//zoom in
            {
                zoom = zoom + .05f;
                if (drawspace.BackgroundImage != null)
                {
                    drawspace.Size = new System.Drawing.Size((int)((drawspace.BackgroundImage.Width * zoom)), (int)((drawspace.BackgroundImage.Height) * zoom));
                }
                else
                {
                    drawspace.Size = new System.Drawing.Size((int)((drawspaceContainer.Width) * zoom), (int)((drawspaceContainer.Height) * zoom));
                }
                drawspace.Refresh();
                drawspace.Focus();
            }
            else if (e.Delta < 0)
            {
                zoom = zoom - .05f;
                if (zoom <= 0)
                {
                    zoom = 1;
                    if (drawspace.BackgroundImage != null)
                    {
                        drawspace.Size = drawspace.BackgroundImage.Size;
                    }
                    else
                    {
                        drawspace.Size = new System.Drawing.Size((int)((drawspaceContainer.Width - 5)), (int)((drawspaceContainer.Height - 5)));
                    }
                }
                if (drawspace.BackgroundImage != null)
                {
                    drawspace.Size = new System.Drawing.Size((int)((drawspace.BackgroundImage.Width * zoom)), (int)((drawspace.BackgroundImage.Height) * zoom));
                }
                else
                {
                    drawspace.Size = new System.Drawing.Size((int)((drawspaceContainer.Width) * zoom), (int)((drawspaceContainer.Height) * zoom));
                }
                drawspace.Refresh();
                drawspace.Focus();
            }
            drawspace_drawspaceContainer_width_diff = ((double)drawspace.Width - drawspaceContainer.Width);
            drawspace_drawspaceContainer_height_diff = ((double)drawspace.Height - drawspaceContainer.Height);
            drawspaceContainer.AutoScrollPosition = new Point((int)(profsx * drawspace_drawspaceContainer_width_diff), (int)(profsy * drawspace_drawspaceContainer_height_diff));
        }
        private void drawspace_Click(object sender, EventArgs e)
        {
            //console.Focus();
        }


	    public void textBox1_Keydown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                desiredLength = Convert.ToInt32(textBox1.Text);
            }

        }
        private void textBox1_Enter(object sender, EventArgs e)
        {
            textBox1.Clear();
        }
        
        private void openFileDialog1_FileOk(object sender, CancelEventArgs e)
        {

        }

        //**
        protected void makedrawspace(Graphics g)
        {
            //System.Drawing.SolidBrush awesomebrush = new System.Drawing.SolidBrush(bgcolor);
            //g.FillRectangle(awesomebrush, new Rectangle(0, 0, 800, 480));
        }
        private void Form1_Load(object sender, EventArgs e)
        {
            
        }
        private void CreateBoundaryButton_Click_1(object sender, EventArgs e)
        {
            if (Db == null)
            {
                MessageBox.Show("Database not connected. \n Connect a database in the Options menu under Tools.");
            }
            else
            {
                newBlockGroup = new BlockGroup(w.d.GetSelectedLines());
                if (newBlockGroup.isBoundary())
                {
                    createBoundaryForm = new CreateBoundary(ref Db);
                    createBoundaryForm.FormClosed += CreateBoundaryFormClosed; //So, this is like the most amazing thing ever!!! Basically i'm adding an event handler on this form to an event on that form!!! I know Right? It's like some kind of inception shit or something. I can't express how amazing this is!!!
                    createBoundaryForm.addEventHandler(CreateBoundaryDirectionClick);
                    createBoundaryForm.Show();
                }
                else
                {
                    MessageBox.Show("Block group not created");
                }
            }
        }
        private void CreateBoundaryDirectionClick(object sender, EventArgs e)
        {
            if (candrawline == false)
            {
                LineGoOrNo();
                scaleSelectButton.Enabled = false;
                toolStripButton7.Enabled = false;
            }
            w.d.selectedLines.Clear();
            w.DrawerEnableMultiSelect(false);
        }
        private void WaterNeed_Click(object sender, EventArgs e)
        {
            Watering form;
            SelectCropForm = new SelectCrop();
            SelectCropForm.ShowDialog();
            string targetCrop = SelectCropForm.TargetCrop;
            SelectCropForm.Dispose();
            form = new Watering(targetCrop);
            form.ShowDialog();
        }
        private void CreateBoundaryFormClosed(object sender, EventArgs e)
        {
            if (createBoundaryForm.isConfirmed)
            {
                
                newBlockGroup.SetBlockGroup(createBoundaryForm.crop, createBoundaryForm.plantSpacing, createBoundaryForm.rowSpacing, createBoundaryForm.tubingNumber);
                FieldLayout.AddBlockGroup(ref newBlockGroup);
                Crop c = FieldLayout.FindCrop(createBoundaryForm.crop);
                if (c == null)
                {
                    c = new Crop(createBoundaryForm.crop, createBoundaryForm.dailyApp);
                    FieldLayout.AddCrop(ref c);
                }
                else
                {
                    c.dailyApp = createBoundaryForm.dailyApp;
                    c.hasBeenUpdated = true;
                }
                createBoundaryForm.Dispose();
            }  
            else if (createBoundaryForm.isEnteringTubingDirection)
            {
                //createBoundaryForm.tubingCoord1 = w.d.GetSelectedLines()[0].pt1;//these both need to be changed once we are able to pass coords from draw space
                //createBoundaryForm.tubingCoord1 = w.d.GetSelectedLines()[0].pt2;//these both need to be changed once we are able to pass coords from draw space
                //createBoundaryForm.Show();
            }
            else
            {
                
            }
        }//Block Group is created from the create boundary form with this event handler which handles an event of another form!!! This shit is so pro, you guys have no idea!!!!!!

        private void CoreyTestConsole_Click(object sender, EventArgs e)
        {
            TestConsole myTest = new TestConsole();
            MessageBox.Show(myTest.TestCreateBoundary());
        }

        private void ListOfBlockGroupsButton_Click(object sender, EventArgs e)
        {
            string output = "";
            int i = 1;
            foreach (BlockGroup b in FieldLayout.allBlockGroups)
            {
                output += "Block " + i.ToString() + "\n";
                output += b.PrintInfo()+"\n";
                i++;
            }
            MessageBox.Show(output);
        }
        private void f3_Click(object sender, EventArgs e)
        {
            FileDialog dlg = new SaveFileDialog();
            DialogResult result = dlg.ShowDialog();

            if (result == DialogResult.OK)
            {
                string path = dlg.FileName;
            }
        }
	private void scaleSelectButton_Click(object sender, EventArgs e)
        {
            if (toolStripButton1.Enabled)
            {
                toolStripButton1.Enabled = false;
                toolStripButton7.Enabled = false;
                scaleSelecting = true;
            }
            else
            {
                toolStripButton1.Enabled = true;
                toolStripButton7.Enabled = true;
                scaleSelecting = false;
            }
            
            ScaleSelectionForm = new SetScale();
            LineGoOrNo();
	}
	private void ListOfCropsButton_Click(object sender, EventArgs e)
        {
            MessageBox.Show(FieldLayout.PrintCrops());
	} 
    }
}
