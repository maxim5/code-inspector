Public Class MemorizedSongs
  Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin

  Public Event NextMenuAsyncCompleted() Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.NextMenuAsyncCompleted
  Public Event GetApplicationSetting(ByVal section As String, ByVal key As String, ByRef value As String, ByRef keyFound As Boolean) Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.GetApplicationSetting

  Private _firstCall As Boolean = True
  Private _secondCall As Boolean = False
  Private _colors As New AvianWaves.AvianPlay.Library.Plugin.Colors
  Private _lang As AvianWaves.AvianPlay.Language.LanguageReader = Nothing
  Dim _items As New List(Of MemorizedItem)

  Public Sub CancelAsync() Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.CancelAsync
    ' No async operations in this plugin -- safe to ignore
  End Sub

  Public Function GetRefreshListString() As String Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.GetRefreshListString
    Return Nothing
  End Function

  Public Function RefreshListFromString(ByVal RefreshString As String) As AvianWaves.AvianPlay.Library.Plugin.iLibraryItem() Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.RefreshListFromString
    Return Nothing
  End Function

  Public ReadOnly Property Items() As AvianWaves.AvianPlay.Library.Plugin.iLibraryItem() Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.Items
    Get
      If Me._secondCall Then
        Return Me._items.ToArray
      Else
        Return Nothing
      End If
    End Get
  End Property

  Public Function NextMenu(ByVal key As String) As Boolean Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.NextMenu
    If Me._firstCall Then
      Me._firstCall = False
      Me._secondCall = True
      Me.LoadMemorizedSongs()
    Else
      Me._secondCall = False ' Set this flag so that Items always returns nothing from here on (no menu progression)

      For Each item As MemorizedItem In Me._items
        If item.Key = key Then
          Dim memdet As New MemorizedDetails

          memdet.BackColor = Me._colors.BackgroundColor
          memdet.ForeColor = Me._colors.ForegroundColor

          memdet.LabelArtist.BackColor = Me._colors.BackgroundColor
          memdet.LabelDate.BackColor = Me._colors.BackgroundColor
          memdet.LabelStation.BackColor = Me._colors.BackgroundColor
          memdet.LabelTitle.BackColor = Me._colors.BackgroundColor
          memdet.LabelArtist.ForeColor = Me._colors.ForegroundColor
          memdet.LabelDate.ForeColor = Me._colors.ForegroundColor
          memdet.LabelStation.ForeColor = Me._colors.ForegroundColor
          memdet.LabelTitle.ForeColor = Me._colors.ForegroundColor

          memdet.TextBoxArtist.BackColor = Me._colors.BackgroundTextboxColor
          memdet.TextBoxDate.BackColor = Me._colors.BackgroundTextboxColor
          memdet.TextBoxStation.BackColor = Me._colors.BackgroundTextboxColor
          memdet.TextBoxTitle.BackColor = Me._colors.BackgroundTextboxColor
          memdet.TextBoxArtist.ForeColor = Me._colors.ForegroundColor
          memdet.TextBoxDate.ForeColor = Me._colors.ForegroundColor
          memdet.TextBoxStation.ForeColor = Me._colors.ForegroundColor
          memdet.TextBoxTitle.ForeColor = Me._colors.ForegroundColor

          memdet.LabelArtist.Text = Me.GetLanguageString("MemorizedSongs_Artist")
          memdet.LabelDate.Text = Me.GetLanguageString("MemorizedSongs_Date")
          memdet.LabelStation.Text = Me.GetLanguageString("MemorizedSongs_Station")
          memdet.LabelTitle.Text = Me.GetLanguageString("MemorizedSongs_Title")
          memdet.MenuItemSearch.Text = Me.GetLanguageString("MemorizedSongs_Search")
          memdet.MenuItemClose.Text = Me.GetLanguageString("Close")

          memdet.TextBoxArtist.Text = item.Artist
          memdet.TextBoxTitle.Text = item.Title
          memdet.TextBoxDate.Text = item.Date
          memdet.TextBoxStation.Text = item.Station

          memdet.ShowDialog()

          Exit For
        End If
      Next
    End If

    Return True ' No async processing
  End Function

  Public Function PreviousMenu() As Boolean Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.PreviousMenu
    Return False ' This only has one level of menu, so a 'back' operation will always leave the plugin
  End Function

  Public Sub LoadMemorizedSongs()
    Me._items.Clear()
    Dim memPath As String = String.Empty
    Dim keyFound As Boolean = False
    RaiseEvent GetApplicationSetting("General", "MemorizedSongsPath", memPath, keyFound)
    If keyFound AndAlso memPath.Length > 0 AndAlso IO.File.Exists(memPath) Then
      Try
        Using fs As New IO.FileStream(memPath, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read)
          Using sr As New IO.StreamReader(fs)
            Dim item As New MemorizedItem
            Dim line As String = String.Empty

            While Not sr.EndOfStream
              line = sr.ReadLine().Trim

              If line.Length > 0 Then
                If String.Compare(Left(line, 7), "Artist:", True) = 0 Then
                  item.Artist = Mid(line, 8).Trim
                ElseIf String.Compare(Left(line, 6), "Title:", True) = 0 Then
                  item.Title = Mid(line, 7).Trim
                ElseIf String.Compare(Left(line, 5), "Song:", True) = 0 Then ' For anybody (me!) using my old PocketPlayer plugin...
                  item.Title = Mid(line, 6).Trim
                ElseIf String.Compare(Left(line, 5), "Time:", True) = 0 Then
                  item.Date = Mid(line, 6).Trim
                ElseIf String.Compare(Left(line, 10), "Date/Time:", True) = 0 Then ' For anybody (me!) using my old PocketPlayer plugin...
                  item.Date = Mid(line, 11).Trim
                ElseIf String.Compare(Left(line, 8), "Station:", True) = 0 Then
                  item.Station = Mid(line, 9).Trim
                End If
              ElseIf item.HasData Then ' If the line is blank, this separates entries, so add it to the list
                Me._items.Add(item)
                item = New MemorizedItem
              Else
                item = New MemorizedItem ' No data!  Start over...
              End If
            End While
          End Using
        End Using
      Catch ex As Exception
        Windows.Forms.MessageBox.Show(GetLanguageString("MemorizedSongs_LoadError") & " " & ex.Message, "Avian Play", Windows.Forms.MessageBoxButtons.OK, Windows.Forms.MessageBoxIcon.Hand, Windows.Forms.MessageBoxDefaultButton.Button1)
      Finally
        Me._items.Reverse()
      End Try
    End If
  End Sub

  Public Sub SetColors(ByVal col As AvianWaves.AvianPlay.Library.Plugin.Colors) Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.SetColors
    Me._colors = col
  End Sub

  Public Sub SetLanguageStrings(ByVal lang As AvianWaves.AvianPlay.Language.LanguageReader) Implements AvianWaves.AvianPlay.Library.Plugin.iLibraryPlugin.SetLanguageStrings
    Me._lang = lang
  End Sub

  Public Function GetLanguageString(ByVal key As String) As String
    If Me._lang IsNot Nothing Then
      Return Me._lang.GetString(key)
    End If
    Return String.Empty
  End Function
End Class
