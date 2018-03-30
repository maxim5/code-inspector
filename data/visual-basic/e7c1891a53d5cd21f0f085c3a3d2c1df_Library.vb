Imports iTunesLib

Namespace Models

    Public Class Library

        Public Tracks As Tracks
        Public Playlists As Playlists

        Public Sub New(iTunesApp As iTunesApp)

            Me.Tracks = New Tracks(iTunesApp.LibraryPlaylist.Tracks)
            Me.Playlists = New Playlists(iTunesApp.LibrarySource.Playlists)

        End Sub

    End Class

End Namespace
