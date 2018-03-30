''' <summary>
''' Developer Name : Mr.Teeradech Janwimalueng (http://teedech.blogspot.com, Thai Language Blog)
''' Date Created : 27 August 2007
''' Last Updated : 23 September 2007
''' License : Microsoft Permissive License (Ms-PL) v1.1
''' Version 1.0
''' </summary>
''' <remarks>
''' 23 September 2007
''' Change on line 22 and 58 for delete CInt because Asc return Integer already.
''' 25 September 2007
''' Fix function decrypt for wrong decrypttion (not loop alphabet) at line 66 and 73 (74 after changed).
''' </remarks>

Public Class CeasarShiftCipherEngine

    Public Function Encrypt(ByVal PlainText As String, ByVal Key As Integer) As String
        'convert plaintext(String) to array of Char
        Dim PlainChar() As Char = PlainText.ToCharArray()
        Dim Ascii(PlainChar.Length) As Integer

        For Count As Integer = 0 To PlainChar.Length - 1
            'convert Char to ASCII number
            Ascii(Count) = Asc(PlainChar(Count))

            'Filter A-Z and a-z only
            'A-Z in ASCII is 65-90
            'a-z in ASCII is 97-122
            If Ascii(Count) >= 65 And Ascii(Count) <= 90 Then
                Ascii(Count) = ((Ascii(Count) - 65 + Key) Mod 26) + 65

                'for easy learning
                'Ascii(Count) -= 65 'assign A-Z as 0 to 25 in order
                'Ascii(Count) = (Ascii(Count) + Key) Mod 26
                'Ascii(Count) += 65
            ElseIf Ascii(Count) >= 97 And Ascii(Count) <= 122 Then
                Ascii(Count) = ((Ascii(Count) - 97 + Key) Mod 26) + 97

                'for easy learning
                'Ascii(Count) -= 97 'assign a-z as 0 to 25 in order
                'Ascii(Count) = (Ascii(Count) + Key) Mod 26
                'Ascii(Count) += 97
            End If

            'convert ASCII to Char
            PlainChar(Count) = Chr(Ascii(Count))
        Next

        'return array of Char as String
        Return PlainChar
    End Function

    Public Function Decrypt(ByVal CipherText As String, ByVal Key As Integer) As String
        'convert ciphertext(String) to array of Char
        Dim CipherChar() As Char = CipherText.ToCharArray()
        Dim Ascii(CipherChar.Length) As Integer

        For Count As Integer = 0 To CipherChar.Length - 1
            'convert Char to ASCII number
            Ascii(Count) = Asc(CipherChar(Count))

            'Filter A-Z and a-z only
            'A-Z in ASCII is 65-90
            'a-z in ASCII is 97-122
            If Ascii(Count) >= 65 And Ascii(Count) <= 90 Then
                Ascii(Count) = ((Ascii(Count) - 65 - (Key Mod 26) + 26)) Mod 26 + 65

                'code below for explain 
                'Ascii(Count) -= 65 'assign A-Z as 0 to 25 in order
                'Key = Key Mod 26
                'Ascii(Count) = (Ascii(Count) - Key + 26) Mod 26
                'Ascii(Count) += 65
            ElseIf Ascii(Count) >= 97 And Ascii(Count) <= 122 Then
                Ascii(Count) = (((Ascii(Count) - 97 - (Key Mod 26) + 26)) Mod 26) + 97

                'code below for explain 
                'Ascii(Count) -= 97 'assign a-z as 0 to 25 in order
                'Key = Key Mod 26
                'Ascii(Count) = (Ascii(Count) - Key + 26)) Mod 26
                'Ascii(Count) += 97
            End If

            'convert ASCII to Char
            CipherChar(Count) = Chr(Ascii(Count))
        Next

        'return array of Char as String
        Return CipherChar
    End Function

End Class
