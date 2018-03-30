Imports System.Security.Cryptography
Imports System.IO
Imports System.Text
Imports System

''' <summary>
'''     Provides encryption services.
''' </summary>
Public Class Encryption_Routines

    ''' <summary>
    '''     Encrypts a string into a 128 bits encrypted string
    ''' </summary>
    ''' <param name="vstrTextToBeEncrypted">The text to be encrypted</param>
    ''' <param name="vstrEncryptionKey">The Key to be use in the encryption</param>
    ''' <returns>A string with a 128 bits encrypted string</returns>
    Public Function EncryptString128Bit(ByVal vstrTextToBeEncrypted As String, _
                                        ByVal vstrEncryptionKey As String) As String
        Return Encryption_Routines.EncryptString128BitShared(vstrTextToBeEncrypted, vstrEncryptionKey)
    End Function

    Public Shared Function EncryptString128BitShared(ByVal vstrTextToBeEncrypted As String, _
                                    ByVal vstrEncryptionKey As String) As String
        If vstrTextToBeEncrypted Is Nothing Then Return String.Empty
        If vstrTextToBeEncrypted.Trim = "" Then Return String.Empty

        Dim bytValue() As Byte
        Dim bytKey() As Byte
        Dim bytEncoded() As Byte
        Dim bytIV() As Byte = {121, 241, 10, 1, 132, 74, 11, 39, 255, 91, 45, 78, 14, 211, 22, 62}
        Dim intLength As Integer
        Dim intRemaining As Integer
        Dim objMemoryStream As New MemoryStream
        Dim objCryptoStream As CryptoStream
        Dim objRijndaelManaged As RijndaelManaged

        '   **********************************************************************
        '   ******  Strip any null character from string to be encrypted    ******
        '   **********************************************************************

        vstrTextToBeEncrypted = Encryption_Routines.StripNullCharactersShared(vstrTextToBeEncrypted)

        '   **********************************************************************
        '   ******  Value must be within ASCII range (i.e., no DBCS chars)  ******
        '   **********************************************************************

        bytValue = Encoding.ASCII.GetBytes(vstrTextToBeEncrypted.ToCharArray)

        intLength = Len(vstrEncryptionKey)

        '   ********************************************************************
        '   ******   Encryption Key must be 256 bits long (32 bytes)      ******
        '   ******   If it is longer than 32 bytes it will be truncated.  ******
        '   ******   If it is shorter than 32 bytes it will be padded     ******
        '   ******   with upper-case Xs.                                  ****** 
        '   ********************************************************************

        If intLength >= 32 Then
            vstrEncryptionKey = Strings.Left(vstrEncryptionKey, 32)
        Else
            intLength = Len(vstrEncryptionKey)
            intRemaining = 32 - intLength
            vstrEncryptionKey = vstrEncryptionKey & Strings.StrDup(intRemaining, "X")
        End If

        bytKey = Encoding.ASCII.GetBytes(vstrEncryptionKey.ToCharArray)

        objRijndaelManaged = New RijndaelManaged
        bytEncoded = Nothing

        '   ***********************************************************************
        '   ******  Create the encryptor and write value to it after it is   ******
        '   ******  converted into a byte array                              ******
        '   ***********************************************************************

        Try

            objCryptoStream = New CryptoStream(objMemoryStream, _
              objRijndaelManaged.CreateEncryptor(bytKey, bytIV), _
              CryptoStreamMode.Write)
            objCryptoStream.Write(bytValue, 0, bytValue.Length)

            objCryptoStream.FlushFinalBlock()

            bytEncoded = objMemoryStream.ToArray
            objMemoryStream.Close()
            objCryptoStream.Close()
        Catch

        End Try

        '   ***********************************************************************
        '   ******   Return encryptes value (converted from  byte Array to   ******
        '   ******   a base64 string).  Base64 is MIME encoding)             ******
        '   ***********************************************************************

        Return Convert.ToBase64String(bytEncoded)

    End Function

    ''' <summary>
    '''     Decrypts a 128 bit string into a regular string.
    ''' </summary>
    ''' <param name="vstrStringToBeDecrypted">The string to decrypt</param>
    ''' <param name="vstrDecryptionKey">The public key needed to decrypt the string</param>
    ''' <returns>The decoded string</returns>
    Public Function DecryptString128Bit(ByVal vstrStringToBeDecrypted As String, _
                                        ByVal vstrDecryptionKey As String) As String
        Return Encryption_Routines.DecryptString128BitShared(vstrStringToBeDecrypted, vstrDecryptionKey)
    End Function

    Public Shared Function DecryptString128BitShared(ByVal vstrStringToBeDecrypted As String, _
                                    ByVal vstrDecryptionKey As String) As String
        If vstrStringToBeDecrypted Is Nothing Then Return String.Empty
        If vstrStringToBeDecrypted.Trim = "" Then Return String.Empty

        Dim bytDataToBeDecrypted() As Byte
        Dim bytTemp() As Byte
        Dim bytIV() As Byte = {121, 241, 10, 1, 132, 74, 11, 39, 255, 91, 45, 78, 14, 211, 22, 62}
        Dim objRijndaelManaged As New RijndaelManaged
        Dim objMemoryStream As MemoryStream
        Dim objCryptoStream As CryptoStream
        Dim bytDecryptionKey() As Byte

        Dim intLength As Integer
        Dim intRemaining As Integer
        Dim strReturnString As String = String.Empty        

        '   *****************************************************************
        '   ******   Convert base64 encrypted value to byte array      ******
        '   *****************************************************************


        '   ***********************************************************************
        '   ******  Create the decryptor and write value to it after it is   ******
        '   ******  converted into a byte array                              ******
        '   ***********************************************************************

        Try
            bytDataToBeDecrypted = Convert.FromBase64String(vstrStringToBeDecrypted)

            '   ********************************************************************
            '   ******   Encryption Key must be 256 bits long (32 bytes)      ******
            '   ******   If it is longer than 32 bytes it will be truncated.  ******
            '   ******   If it is shorter than 32 bytes it will be padded     ******
            '   ******   with upper-case Xs.                                  ****** 
            '   ********************************************************************

            intLength = Len(vstrDecryptionKey)

            If intLength >= 32 Then
                vstrDecryptionKey = Strings.Left(vstrDecryptionKey, 32)
            Else
                intLength = Len(vstrDecryptionKey)
                intRemaining = 32 - intLength
                vstrDecryptionKey = vstrDecryptionKey & Strings.StrDup(intRemaining, "X")
            End If

            bytDecryptionKey = Encoding.ASCII.GetBytes(vstrDecryptionKey.ToCharArray)

            ReDim bytTemp(bytDataToBeDecrypted.Length)

            objMemoryStream = New MemoryStream(bytDataToBeDecrypted)

            objCryptoStream = New CryptoStream(objMemoryStream, _
               objRijndaelManaged.CreateDecryptor(bytDecryptionKey, bytIV), _
               CryptoStreamMode.Read)

            objCryptoStream.Read(bytTemp, 0, bytTemp.Length)

            'objCryptoStream.FlushFinalBlock()
            objMemoryStream.Close()
            objCryptoStream.Close()

        Catch ex As Exception
            Dim strEx As String = ex.ToString
            Return String.Empty
        End Try

        '   *****************************************
        '   ******   Return decrypted value     ******
        '   *****************************************

        Return StripNullCharactersShared(Encoding.ASCII.GetString(bytTemp))

    End Function
    ''' <summary>
    '''     When builiding the 128 bit string, some null characters were added to the string.
    '''     This function will remove them.
    ''' </summary>
    ''' <param name="vstrStringWithNulls">The string to clean</param>
    ''' <returns>The clean string.</returns>
    Private Function StripNullCharacters(ByVal vstrStringWithNulls As String) As String
        Return Encryption_Routines.StripNullCharactersShared(vstrStringWithNulls)
    End Function

    Private Shared Function StripNullCharactersShared(ByVal vstrStringWithNulls As String) As String
        Dim intPosition As Integer
        Dim strStringWithOutNulls As String

        If vstrStringWithNulls Is Nothing Or vstrStringWithNulls = String.Empty Then
            Return ""
        End If

        intPosition = 1
        strStringWithOutNulls = vstrStringWithNulls

        Do While intPosition > 0
            intPosition = InStr(intPosition, vstrStringWithNulls, vbNullChar)

            If intPosition > 0 Then
                strStringWithOutNulls = Left$(strStringWithOutNulls, intPosition - 1) & _
                                        Right$(strStringWithOutNulls, Len(strStringWithOutNulls) - intPosition)
            End If

            If intPosition > strStringWithOutNulls.Length Then
                Exit Do
            End If
        Loop

        Return strStringWithOutNulls
    End Function

    ''' <summary>
    '''     Encrypts a string using MD5. To do so it uses the routines provided in .NET
    ''' </summary>
    ''' <param name="strSource">The string to encrypt</param>
    ''' <returns>A Valid MD5 string of strSource</returns>
    Public Function EncryptStringToMD5(ByVal strSource As String, Optional ByVal boolLower As Boolean = False) As String
        'Converts a regular string into a MD5 encoded-encrypted string.
        If strSource Is Nothing Then Return String.Empty
        If strSource.Trim = "" Then Return String.Empty

        Dim md5Hasher As New MD5CryptoServiceProvider
        Dim hashedBytes As Byte()
        Dim encoder As New System.Text.ASCIIEncoding
        'Dim encoder As New UnicodeEncoding

        'The line below was commented for testing purposes.
        'hashedBytes = md5Hasher.ComputeHash(encoder.GetBytes(strSource))
        hashedBytes = CType(CryptoConfig.CreateFromName("MD5"), HashAlgorithm).ComputeHash(encoder.GetBytes(strSource))


        ' convert hash value to hex string
        Dim sb As New System.Text.StringBuilder
        Dim outputByte As Byte
        For Each outputByte In hashedBytes
            ' convert each byte to a Hexadecimal upper case string
            'These 2 lines return the same string.
            'BitConverter.ToString(hashedBytes)
            If boolLower Then
                sb.Append(outputByte.ToString("x2"))
            Else
                sb.Append(outputByte.ToString("X2"))
            End If
        Next 'outputByte
        Return sb.ToString
    End Function

    ''' <summary>
    '''     Encrypts a File using PGP.
    '''     To do so, a free application is being used and needs to be install in c:\gnupg\
    '''     The directory needs to have access rights to everyone.
    '''     it uses GNUGPG.
    ''' </summary>
    ''' <param name="strFilePath">The route to the file to encrypt</param>
    ''' <param name="strEncryptedFile">The route to the encrypted file</param>
    ''' <returns>True if the encryption was sucessfull. False otherwise.</returns>
    Public Function EncryptAFileUsingPGP(ByVal strFilePath As String, _
                                         ByRef strEncryptedFile As String) As Boolean
        'Runs the gpg command line tool.
        'Returns true if the file was encoded sucessfully.
        'Returns false otherwise

        Dim iID As Integer
        Dim strShellCmd As String
        Try
            strShellCmd = String.Concat("c:\gnupg\gpg.exe -o ", strFilePath, ".PGP -e --yes -r Cleartech ", strFilePath)
            iID = Shell(strShellCmd, AppWinStyle.NormalFocus, True, 100000)
            If iID = 0 Then
                'means that the application has finished and the file should be there by now.
                'We should verified that the file is here by bow
                If File.Exists(strFilePath + ".PGP") Then
                    strEncryptedFile = strFilePath + ".PGP"
                    Return True
                End If
            End If

            'Something happened and the file couldn't not be created after 100 Minutes
            strEncryptedFile = String.Empty
            Return False
        Catch exc As FileNotFoundException
            Log.appendToLog(exc)            

            strEncryptedFile = String.Empty
            Return False
        End Try

    End Function

    Public Function EncryptWithExternalKey(ByVal strValue As String, ByVal strFilePath As String, ByVal strFileName As String, Optional ByVal strPrefix As String = "")
        If strValue Is Nothing Then Return String.Empty
        If strValue.Trim = "" Then Return String.Empty

        Dim oParser As New RegularExpressionParser( _
            strFileName, _
            "(?<1>\w*)" _
        ) : Dim oHash As Hashtable = oParser.ParseRegularResponse
        strFileName = oHash("1").ToString() & ".cfg"
        'parse end
        Return Me.EncryptString128Bit(strValue, New FileRead().GetFileContent(strFilePath & strFileName) & strPrefix)
    End Function

    Public Shared Function DecodeBase64(ByVal base64String As String) As String
        If base64String Is Nothing Then Return String.Empty
        If base64String.Trim = "" Then Return String.Empty
        ' Convert the Base64 UUEncoded input into binary output.
        'Dim ascii As New ASCIIEncoding
        'Return ascii.GetString(System.Convert.FromBase64String(base64String))
        Dim utf As New System.Text.UTF8Encoding
        Return utf.GetString(System.Convert.FromBase64String(base64String))
    End Function

    Public Shared Function EncryptBase64(ByVal base64String As String) As String

        If base64String Is Nothing Then Return String.Empty
        If base64String.Trim = "" Then Return String.Empty

        Dim ae As ASCIIEncoding = New ASCIIEncoding
        Dim data() As Byte = ae.GetBytes(base64String)
        Return System.Convert.ToBase64String(data)

    End Function

    Public Function SHA1digest(ByVal message As String) As String
        If message Is Nothing Then Return String.Empty
        If message.Trim = "" Then Return String.Empty

        'Get the hash
        Dim sha As New SHA1CryptoServiceProvider
        Dim ae As ASCIIEncoding = New ASCIIEncoding
        Dim data() As Byte = ae.GetBytes(message)
        Dim digest() As Byte = sha.ComputeHash(data)

        'Convert to # string
        Dim s As StringBuilder = New StringBuilder
        Dim length As Integer = digest.Length
        Dim n As Integer
        For n = 0 To length - 1 Step n + 1
            s.Append(CType(digest(n), Integer))
            If n <> length - 1 Then
                s.Append(" "c)
            End If
        Next
        Return s.ToString()
    End Function

    Public Function SHA1exadecimal(ByVal message As String)
        If message Is Nothing Then Return String.Empty
        If message.Trim = "" Then Return String.Empty

        'Get the hash
        Dim sha As New SHA1CryptoServiceProvider
        Dim ae As ASCIIEncoding = New ASCIIEncoding
        Dim data() As Byte = ae.GetBytes(message)
        Dim digest() As Byte = sha.ComputeHash(data)

        'Convert to # string
        Dim s As StringBuilder = New StringBuilder
        Dim length As Integer = digest.Length
        Dim n As Integer

        s = New StringBuilder
        For n = 0 To length - 1 Step n + 1
            s.Append(String.Format("{0,2:x}", digest(n)).Replace(" ", "0"))
        Next
        Return s.ToString()
    End Function

    'System.Security.Cryptography.MACTripleDES
    'A MAC can be used to determine whether a message sent over an insecure channel has been tampered with, provided that the sender and receiver share a secret key. The sender computes the MAC for the original data, and sends both as a single message. The receiver recomputed the MAC on the received message, and checks that the computed MAC matches the transmitted MAC.
    'MACTripleDES uses a key of length 8, 16 or 24 bytes, and produces a hash sequence of length 8 bytes.
    'strKey must be 16 characters long
    Public Function EncryptMACTripleDES(ByVal ClearString As String, _
                ByVal strKey As String) As String
        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim ae As ASCIIEncoding = New ASCIIEncoding
        'Dim ae As UnicodeEncoding = New UnicodeEncoding
        Dim Key() As Byte = {}
        Key = ae.GetBytes(strKey)

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim des As New System.Security.Cryptography.MACTripleDES(Key)
        Dim hash() As Byte = des.ComputeHash(bytClearString)
        Return Convert.ToBase64String(hash)
    End Function

    'System.Security.Cryptography.HMACSHA1
    'HMACSHA1 accepts keys of any size, and produces a hash sequence of length 20 bytes.
    Public Function EncryptHMACSHA1(ByVal ClearString As String, _
        ByVal strKey As String) As String

        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim ae As ASCIIEncoding = New ASCIIEncoding
        'Dim ae As UnicodeEncoding = New UnicodeEncoding
        Dim Key() As Byte = {}
        Key = ae.GetBytes(strKey)

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim shaM As New SHA1Managed
        Dim hmac As New HMACSHA1(Key)
        Dim cs As New CryptoStream(Stream.Null, shaM, CryptoStreamMode.Write)
        cs.Write(bytClearString, 0, bytClearString.Length)
        cs.Close()
        Dim hash() As Byte = shaM.Hash
        Return Convert.ToBase64String(hash)
    End Function

    'As I said there is another type of Hash algorithms. It is key-independent type that involves five hash classes.
    'In the following examples I show you, how you can use them in your app.
    'System.Security.Cryptography.SHA1
    'This class consists of two sub classes that implement SHA1 hash.  SHA1CryptoServiceProvider that computes the SHA1 hash for the input data using the implementation provided by the cryptographic service provider (CSP) and SHA1Managed that is a purely managed implementation of SHA1 that does not wrap CAPI.
    'The hash size for the SHA1 algorithm is 160 bits.
    Public Function EncryptSHA1CSP(ByVal ClearString As String) As String
        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim sha As New _
        System.Security.Cryptography.SHA1CryptoServiceProvider
        Dim hash() As Byte = sha.ComputeHash(bytClearString)
        Return Convert.ToBase64String(hash)
    End Function

    Public Function EncryptSHA1Managed(ByVal ClearString As String) As String
        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim sha As New _
        System.Security.Cryptography.SHA1Managed
        Dim hash() As Byte = sha.ComputeHash(bytClearString)
        Return Convert.ToBase64String(hash)
    End Function

    'System.Security.Cryptography.SHA256
    'The hash size for SHA256 algorithm is 256 bits and the only implementation of this class is SHA256Managed.
    Public Function EncryptSHA256Managed(ByVal ClearString As String) As String
        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim sha As New _
            System.Security.Cryptography.SHA256Managed
        Dim hash() As Byte = sha.ComputeHash(bytClearString)
        Return Convert.ToBase64String(hash)
    End Function

    'System.Security.Cryptography.SHA384
    'The hash size for SHA384 algorithm is 384 bits and the only implementation of this class is SHA384Managed.
    Public Function EncryptSHA384Managed(ByVal ClearString As String) As String
        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim sha As New _
        System.Security.Cryptography.SHA384Managed
        Dim hash() As Byte = sha.ComputeHash(bytClearString)
        Return Convert.ToBase64String(hash)
    End Function

    'System.Security.Cryptography.SHA512
    'The hash size for SHA512 algorithm is 512 bits and the only implementation of this class is SHA512Managed.
    Public Function EncryptSHA512Managed(ByVal ClearString As String) As String
        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim sha As New _
        System.Security.Cryptography.SHA512Managed
        Dim hash() As Byte = sha.ComputeHash(bytClearString)
        Return Convert.ToBase64String(hash)
    End Function

    'System.Security.Cryptography.MD5
    'The only implementation of this class is SHA1CryptoServiceProvider.
    Public Function EncryptMD5CSP(ByVal ClearString As String) As String
        If ClearString Is Nothing Then Return String.Empty
        If ClearString.Trim = "" Then Return String.Empty

        Dim uEncode As New UnicodeEncoding
        Dim bytClearString() As Byte = uEncode.GetBytes(ClearString)
        Dim sha As New _
        System.Security.Cryptography.MD5CryptoServiceProvider
        Dim hash() As Byte = sha.ComputeHash(bytClearString)
        Return Convert.ToBase64String(hash)
    End Function

    Public Shared Function NumericEncryptDecrypt(ByVal strKey As String, ByVal strText As String, ByVal iIncrement As Integer, ByVal boolDecrypt As Boolean) As Decimal
        '>>>>>>>>>>> validaciones de entrada
        'la clave debe tener 10 digitos del 0 al 9, en cualquier orden, no se pueden repetir digitos
        'el incremento debe ser menor a 10, diferente de 5, default es 1 si se envia 0
        If iIncrement = 0 Then iIncrement = 1
        If iIncrement >= 10 Or iIncrement = 5 Then Return Nothing
        If Trim(strKey) = Nothing Then Return Nothing
        RegularExpressionParser.TextStripNullChars(strKey)
        If Trim(strText) = Nothing Then Return Nothing
        If Not IsNumeric(strText) Then Return Nothing
        RegularExpressionParser.TextStripNullChars(strText)
        '>>>>>>>>>>>
        Dim chrKey() As Char = strKey
        Dim chrText() As Char = strText
        Dim i As Integer
        Dim iIndex As Integer
        Dim chrIndex As Char
        Dim strResult As String = String.Empty
        Dim strKeyValid As String = String.Empty
        '>>>>>>>>>>> Validate Key
        If strKey.Length <> 10 Then Return Nothing
        If Not IsNumeric(strKey) Then Return Nothing
        For i = 0 To chrKey.Length - 1
            If InStr(strKeyValid, Char.ToString(chrKey(i))) > 0 Then Return Nothing
            strKeyValid &= Char.ToString(chrKey(i))
        Next
        '>>>>>>>>>>>
        For i = 0 To strText.Length - 1
            If boolDecrypt Then
                chrIndex = chrText(i)
                strResult &= Array.IndexOf(chrKey, chrIndex).ToString
            Else
                iIndex = CInt(chrText(i).ToString())
                strResult &= Char.ToString(chrKey(iIndex))
            End If
            strKey = strKey.Substring(iIncrement) & strKey.Substring(0, iIncrement)
            chrKey = strKey
        Next
        Return CDec(strResult)
    End Function

    Public Shared Function NumericDecrypt(ByVal strKey As String, ByVal strText As String, ByVal iIncrement As Integer) As Decimal
        Return NumericEncryptDecrypt(strKey, strText, iIncrement, True)
    End Function

    Public Shared Function NumericEncrypt(ByVal strKey As String, ByVal strText As String, ByVal iIncrement As Integer) As Decimal
        Return NumericEncryptDecrypt(strKey, strText, iIncrement, False)
    End Function

    'Public Shared Function EncryptRefID(ByVal strRefId As String) As String
    '    Dim strEncryptonNum As String
    '    Dim bEncrypNum As Boolean
    '    Try
    '        strEncryptonNum = New wConfigBase_CTRL().CFG_EncryptionNum
    '        If Not strEncryptonNum Is Nothing And strEncryptonNum <> String.Empty Then
    '            If strEncryptonNum.Length = 10 Then
    '                bEncrypNum = True
    '                strRefId = CStr(Encryption_Routines.NumericEncrypt(strEncryptonNum, strRefId, 0))
    '            End If
    '        End If
    '        If Not bEncrypNum Then CashierInterface.Log.appendToLog(CashierInterface.Log.LEVEL_DEBUG, "The Method wConfigBase_CTRL().CFG_EncryptionNum this Returning Nothing Or is < 10. Look Up-> CCScreenCheck.vb->EncryptRefID, Please verify")
    '        Return strRefId
    '    Catch ex As Exception
    '        CashierInterface.Log.appendToLog(CashierInterface.Log.LEVEL_DEBUG, String.Concat(System.Reflection.MethodBase.GetCurrentMethod.Name, " - ", System.Reflection.MethodBase.GetCurrentMethod.DeclaringType.Name, " - ", System.Reflection.MethodBase.GetCurrentMethod.DeclaringType.Namespace, vbCrLf, ex.ToString()))
    '        Return String.Empty
    '    End Try
    'End Function


    'Public Shared Function EncryptRC4Salaa(ByVal strKey As String, ByVal strText As String) As String
    '    'Ejemplo: StringToHexString(crpt.Salaa(<xml><tpPago>C</tpPago><amount>0.01</amount><urlResponse>www.sitiocliente.com.mx</urlResponse><referencia>NUM_FACTURA</referencia><moneda>MXN</moneda><xml>,SEMILLA))
    '    If strKey = Nothing Or strText = Nothing Then
    '        Return Nothing
    '    End If
    '    Dim oEncrypt As New CSUtil.Encryption.rc4
    '    Dim strResult As String = oEncrypt.Salaa(strText, strKey)
    '    strResult = oEncrypt.StringToHexString(strResult)
    '    oEncrypt = Nothing
    '    Return strResult
    'End Function

End Class
