<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class HashFromFile
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.browseForFileButton = New System.Windows.Forms.Button()
        Me.fileLocationTextbox = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.hashTypeCombobox = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.getFileHashButton = New System.Windows.Forms.Button()
        Me.fileHashTextbox = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog()
        Me.SuspendLayout()
        '
        'browseForFileButton
        '
        Me.browseForFileButton.Location = New System.Drawing.Point(337, 26)
        Me.browseForFileButton.Name = "browseForFileButton"
        Me.browseForFileButton.Size = New System.Drawing.Size(59, 22)
        Me.browseForFileButton.TabIndex = 0
        Me.browseForFileButton.Text = "Browse..."
        Me.browseForFileButton.UseVisualStyleBackColor = True
        '
        'fileLocationTextbox
        '
        Me.fileLocationTextbox.Location = New System.Drawing.Point(12, 28)
        Me.fileLocationTextbox.Name = "fileLocationTextbox"
        Me.fileLocationTextbox.Size = New System.Drawing.Size(319, 20)
        Me.fileLocationTextbox.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 9)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(68, 13)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Select a File:"
        '
        'hashTypeCombobox
        '
        Me.hashTypeCombobox.FormattingEnabled = True
        Me.hashTypeCombobox.Items.AddRange(New Object() {"-----Cryptographic Hashes-----", "", "GOST", "HAS-160", "MD2", "MD4", "MD5", "RIPEMD128", "RIPEMD160", "RIPEMD256", "RIPEMD320", "SHA-1", "SHA-224", "SHA-256", "SHA-384", "SHA-512", "WHIRLPOOL", "", "-----SHA-3 Implementations-----", "", "--Blake Implementation--", "Blake-224", "Blake-256", "Blake-384", "Blake-512", "", "--Blue Midnight Wish Implementation--", "BlueMidnightWish-224", "BlueMidnightWish-256", "BlueMidnightWish-384", "BlueMidnightWish-512", "", "--Cube Hash Implementation--", "CubeHash-224", "CubeHash-256", "CubeHash-384", "CubeHash-512", "", "--Echo Implementation--", "Echo-224", "Echo-256", "Echo-384", "Echo-512", "", "--Fugue Implementation--", "Fugue-224", "Fugue-256", "Fugue-384", "Fugue-512", "", "--Groestl Implementation--", "Groestl-224", "Groestl-256", "Groestl-384", "Groestl-512", "", "--Hamsi Implementation--", "Hamsi-224", "Hamsi-256", "Hamsi-384", "Hamsi-512", "", "--JH Implementation--", "JH-224", "JH-256", "JH-384", "JH-512", "", "--Keccak Implementation--", "Keccak-224", "Keccak-256", "Keccak-384", "Keccak-512", "", "--Luffa Implementation--", "Luffa-224", "Luffa-256", "Luffa-384", "Luffa-512", "", "--Shabal Implementation--", "Shabal-224", "Shabal-256", "Shabal-384", "Shabal-512", "", "--Shavite Implementation--", "Shavite-224", "Shavite-256", "Shavite-384", "Shavite-512", "", "--SIMD Implementation--", "SIMD-224", "SIMD-256", "SIMD-384", "SIMD-512", "", "--Skein Implementation--", "Skein-224", "Skein-256", "Skein-384", "Skein-512", "", "-----Non-Cryptographic Hashes-----", "AP", "Bernstein", "Bernstein1", "BKDR", "DEK", "DJB", "ELF", "FNV", "FNV1a", "Jenkins3", "JS", "Murmur2", "OneAtATime", "PJW", "Rotating", "RS", "SBDM", "ShiftAndXor", "SuperFast"})
        Me.hashTypeCombobox.Location = New System.Drawing.Point(12, 83)
        Me.hashTypeCombobox.Name = "hashTypeCombobox"
        Me.hashTypeCombobox.Size = New System.Drawing.Size(226, 21)
        Me.hashTypeCombobox.TabIndex = 3
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 67)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(120, 13)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "Select hash to produce:"
        '
        'getFileHashButton
        '
        Me.getFileHashButton.Location = New System.Drawing.Point(12, 170)
        Me.getFileHashButton.Name = "getFileHashButton"
        Me.getFileHashButton.Size = New System.Drawing.Size(90, 21)
        Me.getFileHashButton.TabIndex = 5
        Me.getFileHashButton.Text = "Get File Hash"
        Me.getFileHashButton.UseVisualStyleBackColor = True
        '
        'fileHashTextbox
        '
        Me.fileHashTextbox.Location = New System.Drawing.Point(12, 144)
        Me.fileHashTextbox.Name = "fileHashTextbox"
        Me.fileHashTextbox.Size = New System.Drawing.Size(384, 20)
        Me.fileHashTextbox.TabIndex = 6
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(12, 128)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(54, 13)
        Me.Label3.TabIndex = 7
        Me.Label3.Text = "File Hash:"
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.FileName = "OpenFileDialog1"
        '
        'HashFromFile
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(408, 220)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.fileHashTextbox)
        Me.Controls.Add(Me.getFileHashButton)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.hashTypeCombobox)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.fileLocationTextbox)
        Me.Controls.Add(Me.browseForFileButton)
        Me.Name = "HashFromFile"
        Me.Text = "Get hash from a file"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents browseForFileButton As System.Windows.Forms.Button
    Friend WithEvents fileLocationTextbox As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents hashTypeCombobox As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents getFileHashButton As System.Windows.Forms.Button
    Friend WithEvents fileHashTextbox As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
End Class
