Imports System
Imports System.Security.Cryptography
Imports System.Collections.Generic
Imports System.Text
Imports System.Web

Namespace MyspaceToolkit
    Namespace OAuth
        Public Class OAuthBase

            ''' <summary> 
            ''' Provides a predefined set of algorithms that are supported officially by the protocol 
            ''' </summary> 
            Public Enum SignatureTypes
                HMACSHA1
                PLAINTEXT
                RSASHA1
            End Enum

            ''' <summary> 
            ''' Provides an internal structure to sort the query parameter 
            ''' </summary> 
            Protected Class QueryParameter
                Private m_name As String = Nothing
                Private m_value As String = Nothing

                Public Sub New(ByVal name As String, ByVal value As String)
                    Me.m_name = name
                    Me.m_value = value
                End Sub

                Public ReadOnly Property Name() As String
                    Get
                        Return m_name
                    End Get
                End Property

                Public ReadOnly Property Value() As String
                    Get
                        Return m_value
                    End Get
                End Property
            End Class

            ''' <summary> 
            ''' Comparer class used to perform the sorting of the query parameters 
            ''' </summary> 
            Protected Class QueryParameterComparer
                Implements IComparer(Of QueryParameter)

#Region "IComparer Members"

                Public Function Compare(ByVal x As QueryParameter, ByVal y As QueryParameter) As Integer Implements System.Collections.Generic.IComparer(Of QueryParameter).Compare
                    If x.Name = y.Name Then
                        Return String.Compare(x.Value, y.Value)
                    Else
                        Return String.Compare(x.Name, y.Name)
                    End If
                End Function

#End Region
            End Class

            Protected Const OAuthVersion As String = "1.0"
            Protected Const OAuthParameterPrefix As String = "oauth_"

            ' 
            ' List of know and used oauth parameters' names 
            ' 
            Protected Const OAuthConsumerKeyKey As String = "oauth_consumer_key"
            Protected Const OAuthCallbackKey As String = "oauth_callback"
            Protected Const OAuthVersionKey As String = "oauth_version"
            Protected Const OAuthSignatureMethodKey As String = "oauth_signature_method"
            Protected Const OAuthSignatureKey As String = "oauth_signature"
            Protected Const OAuthTimestampKey As String = "oauth_timestamp"
            Protected Const OAuthNonceKey As String = "oauth_nonce"
            Protected Const OAuthTokenKey As String = "oauth_token"
            Protected Const OAuthTokenSecretKey As String = "oauth_token_secret"

            Protected Const HMACSHA1SignatureType As String = "HMAC-SHA1"
            Protected Const PlainTextSignatureType As String = "PLAINTEXT"
            Protected Const RSASHA1SignatureType As String = "RSA-SHA1"

            Protected random As New Random()

            Protected unreservedChars As String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~"

            ''' <summary> 
            ''' Helper function to compute a hash value 
            ''' </summary> 
            ''' <param name="hashAlgorithm">The hashing algoirhtm used. If that algorithm needs some initialization, like HMAC and its derivatives, they should be initialized prior to passing it to this function</param> 
            ''' <param name="data">The data to hash</param> 
            ''' <returns>a Base64 string of the hash value</returns> 
            Private Function ComputeHash(ByVal hashAlgorithm As HashAlgorithm, ByVal data As String) As String
                If hashAlgorithm Is Nothing Then
                    Throw New ArgumentNullException("hashAlgorithm")
                End If

                If String.IsNullOrEmpty(data) Then
                    Throw New ArgumentNullException("data")
                End If

                'Modified encoding to be UTF8 based on recommendations from a myspace employee rajiv 
                ':::Joseph Farrar 
                Dim dataBuffer As Byte() = System.Text.Encoding.UTF8.GetBytes(data)
                Dim hashBytes As Byte() = hashAlgorithm.ComputeHash(dataBuffer)

                Return Convert.ToBase64String(hashBytes)
            End Function

            ''' <summary> 
            ''' Internal function to cut out all non oauth query string parameters (all parameters not begining with "oauth_") 
            ''' </summary> 
            ''' <param name="parameters">The query string part of the Url</param> 
            ''' <returns>A list of QueryParameter each containing the parameter name and value</returns> 
            Private Function GetQueryParameters(ByVal parameters As String) As List(Of QueryParameter)
                If parameters.StartsWith("?") Then
                    parameters = parameters.Remove(0, 1)
                End If

                Dim result As New List(Of QueryParameter)()

                If Not String.IsNullOrEmpty(parameters) Then
                    Dim p As String() = parameters.Split("&")
                    For Each s As String In p
                        If Not String.IsNullOrEmpty(s) AndAlso Not s.StartsWith(OAuthParameterPrefix) Then
                            If s.IndexOf("=") > -1 Then
                                Dim temp As String() = s.Split("=")
                                result.Add(New QueryParameter(temp(0), temp(1)))
                            Else
                                result.Add(New QueryParameter(s, String.Empty))
                            End If
                        End If
                    Next
                End If

                Return result
            End Function

            ''' <summary> 
            ''' This is a different Url Encode implementation since the default .NET one outputs the percent encoding in lower case. 
            ''' While this is not a problem with the percent encoding spec, it is used in upper case throughout OAuth 
            ''' </summary> 
            ''' <param name="value">The value to Url encode</param> 
            ''' <returns>Returns a Url encoded string</returns> 
            Public Function UrlEncode(ByVal value As String) As String
                Dim result As New StringBuilder()

                For Each symbol As Char In value
                    If unreservedChars.IndexOf(symbol) <> -1 Then
                        result.Append(symbol)
                    Else
                        result.Append("%" & String.Format("{0:X2}", CInt(AscW(symbol))))
                    End If
                Next

                Return result.ToString()
            End Function

            ''' <summary> 
            ''' Normalizes the request parameters accoriding to the spec 
            ''' </summary> 
            ''' <param name="parameters">The list of parameters already sorted</param> 
            ''' <returns>a string representing the normalized parameters</returns> 
            Protected Function NormalizeRequestParameters(ByVal parameters As IList(Of QueryParameter)) As String
                Dim sb As New StringBuilder()
                Dim p As QueryParameter = Nothing
                For i As Integer = 0 To parameters.Count - 1
                    p = parameters(i)

                    'Modified concatenation of parameters to be individually UrlEncoded based on recommendations from myspace 
                    ':::Joseph Farrar 
                    sb.Append(String.Format("{0}={1}", UrlEncode(p.Name), UrlEncode(p.Value)))

                    If i < parameters.Count - 1 Then
                        sb.Append("&")
                    End If
                Next

                Return sb.ToString()
            End Function

            ''' <summary> 
            ''' Generate the signature base that is used to produce the signature 
            ''' </summary> 
            ''' <param name="url">The full url that needs to be signed including its non OAuth url parameters</param> 
            ''' <param name="consumerKey">The consumer key</param> 
            ''' <param name="consumerSecret">The consumer seceret</param> 
            ''' <param name="token">The token, if available. If not available pass null or an empty string</param> 
            ''' <param name="tokenSecret">The token secret, if available. If not available pass null or an empty string</param> 
            ''' <param name="httpMethod">The http method used. Must be a valid HTTP method verb (POST,GET,PUT, etc)</param> 
            ''' <param name="signatureType">The signature type. To use the default values use <see cref="OAuthBase.SignatureTypes">OAuthBase.SignatureTypes</see>.</param> 
            ''' <returns>The signature base</returns> 
            Public Function GenerateSignatureBase(ByVal url As Uri, ByVal consumerKey As String, ByVal consumerSecret As String, ByVal token As String, ByVal tokenSecret As String, ByVal httpMethod As String, _
            ByVal timeStamp As String, ByVal nonce As String, ByVal signatureType As String) As String
                If token Is Nothing Then
                    token = String.Empty
                End If

                If tokenSecret Is Nothing Then
                    tokenSecret = String.Empty
                End If

                If String.IsNullOrEmpty(consumerKey) Then
                    Throw New ArgumentNullException("consumerKey")
                End If

                If String.IsNullOrEmpty(consumerSecret) Then
                    Throw New ArgumentNullException("consumerSecret")
                End If

                If String.IsNullOrEmpty(httpMethod) Then
                    Throw New ArgumentNullException("httpMethod")
                End If

                If String.IsNullOrEmpty(signatureType) Then
                    Throw New ArgumentNullException("signatureType")
                End If

                Dim parameters As List(Of QueryParameter) = GetQueryParameters(url.Query)
                parameters.Add(New QueryParameter(OAuthVersionKey, OAuthVersion))
                parameters.Add(New QueryParameter(OAuthNonceKey, nonce))
                parameters.Add(New QueryParameter(OAuthTimestampKey, timeStamp))
                parameters.Add(New QueryParameter(OAuthSignatureMethodKey, signatureType))
                'Added in AuthToken because it is required even if it's blank based on the myspace spec 
                ':::Joseph Farrar 
                parameters.Add(New QueryParameter(OAuthTokenKey, token))
                parameters.Add(New QueryParameter(OAuthConsumerKeyKey, consumerKey))

                'This condition might be perfectly fine, but with the myspace spec the auth token is always necessary even if it's blank 
                ':::Joseph Farrar 
                '--------------------------------- 
                'if (!string.IsNullOrEmpty(token)) 
                '{ 
                ' parameters.Add(new QueryParameter(OAuthTokenKey, token)); 
                '} 

                parameters.Sort(New QueryParameterComparer())

                Dim normalizedRequestParameters As String = NormalizeRequestParameters(parameters)

                'Myspace spec requires only 3 sets of data. The method (GET, POST, PUT, DELETE etc.), The url and the normalized parameters. 
                ':::Joseph Farrar 
                Dim signatureBase As New StringBuilder()
                signatureBase.AppendFormat("{0}&", httpMethod.ToUpper())
                signatureBase.AppendFormat("{0}&", UrlEncode(String.Format("{0}://{1}{2}", url.Scheme, url.Host, url.AbsolutePath)))
                signatureBase.AppendFormat("{0}", UrlEncode(normalizedRequestParameters))

                'This was the orignal method, which is perfectly valid OAuth compliant, it just doesn't work with the myspace spec and has been commented out 
                ':::Joseph Farrar 
                '--------------------------------------------------- 
                'StringBuilder signatureBase = new StringBuilder(); 
                'signatureBase.AppendFormat("{0}&", httpMethod.ToUpper()); 
                'signatureBase.AppendFormat("{0}&", UrlEncode(string.Format("{0}://{1}{2}", url.Scheme, url.Host, url.AbsolutePath))); 
                'signatureBase.AppendFormat("{0}&", UrlEncode(normalizedRequestParameters)); 
                'signatureBase.AppendFormat("{0}&", UrlEncode(consumerSecret)); 
                'signatureBase.AppendFormat("{0}", UrlEncode(tokenSecret)); 

                Return signatureBase.ToString()
            End Function

            ''' <summary> 
            ''' Generate the signature value based on the given signature base and hash algorithm 
            ''' </summary> 
            ''' <param name="signatureBase">The signature based as produced by the GenerateSignatureBase method or by any other means</param> 
            ''' <param name="hash">The hash algorithm used to perform the hashing. If the hashing algorithm requires initialization or a key it should be set prior to calling this method</param> 
            ''' <returns>A base64 string of the hash value</returns> 
            Public Function GenerateSignatureUsingHash(ByVal signatureBase As String, ByVal hash As HashAlgorithm) As String
                Return ComputeHash(hash, signatureBase)
            End Function

            ''' <summary> 
            ''' Generates a signature using the HMAC-SHA1 algorithm 
            ''' </summary> 
            ''' <param name="url">The full url that needs to be signed including its non OAuth url parameters</param> 
            ''' <param name="consumerKey">The consumer key</param> 
            ''' <param name="consumerSecret">The consumer seceret</param> 
            ''' <param name="token">The token, if available. If not available pass null or an empty string</param> 
            ''' <param name="tokenSecret">The token secret, if available. If not available pass null or an empty string</param> 
            ''' <param name="httpMethod">The http method used. Must be a valid HTTP method verb (POST,GET,PUT, etc)</param> 
            ''' <returns>A base64 string of the hash value</returns> 
            Public Function GenerateSignature(ByVal url As Uri, ByVal consumerKey As String, ByVal consumerSecret As String, ByVal token As String, ByVal tokenSecret As String, ByVal httpMethod As String, _
            ByVal timeStamp As String, ByVal nonce As String) As String
                Return GenerateSignature(url, consumerKey, consumerSecret, token, tokenSecret, httpMethod, _
                timeStamp, nonce, SignatureTypes.HMACSHA1)
            End Function

            ''' <summary> 
            ''' Generates a signature using the specified signatureType 
            ''' </summary> 
            ''' <param name="url">The full url that needs to be signed including its non OAuth url parameters</param> 
            ''' <param name="consumerKey">The consumer key</param> 
            ''' <param name="consumerSecret">The consumer seceret</param> 
            ''' <param name="token">The token, if available. If not available pass null or an empty string</param> 
            ''' <param name="tokenSecret">The token secret, if available. If not available pass null or an empty string</param> 
            ''' <param name="httpMethod">The http method used. Must be a valid HTTP method verb (POST,GET,PUT, etc)</param> 
            ''' <param name="signatureType">The type of signature to use</param> 
            ''' <returns>A base64 string of the hash value</returns> 
            Public Function GenerateSignature(ByVal url As Uri, ByVal consumerKey As String, ByVal consumerSecret As String, ByVal token As String, ByVal tokenSecret As String, ByVal httpMethod As String, _
            ByVal timeStamp As String, ByVal nonce As String, ByVal signatureType As SignatureTypes) As String
                Select Case signatureType
                    Case SignatureTypes.PLAINTEXT
                        Return UrlEncode(String.Format("{0}&{1}", consumerSecret, tokenSecret))
                    Case SignatureTypes.HMACSHA1

                        Dim signatureBase As String = GenerateSignatureBase(url, consumerKey, consumerSecret, token, tokenSecret, httpMethod, _
                        timeStamp, nonce, HMACSHA1SignatureType)

                        Dim hmacsha1 As New HMACSHA1()

                        'Modified the method to be UTF8 encoding based on recommendations by myspace. Additionally after much time and debugging, modified this methdo to NOT urlencode the values of consumer secret. 
                        ':::Joseph Farrar 
                        hmacsha1.Key = Encoding.UTF8.GetBytes(consumerSecret + "&")

                        'Original method was url encoding these values 
                        ':::Joseph Farrar 
                        '--------------------------------------------- 
                        ' hmacsha1.Key = Encoding.ASCII.GetBytes(string.Format("{0}&{1}", UrlEncode(consumerSecret), UrlEncode(tokenSecret))); 


                        Return GenerateSignatureUsingHash(signatureBase, hmacsha1)
                    Case SignatureTypes.RSASHA1

                        Throw New NotImplementedException()
                    Case Else
                        Throw New ArgumentException("Unknown signature type", "signatureType")
                End Select
            End Function

            ''' <summary> 
            ''' Generate the timestamp for the signature 
            ''' </summary> 
            ''' <returns></returns> 
            Public Overridable Function GenerateTimeStamp() As String
                Dim ts As TimeSpan = DateTime.UtcNow - New DateTime(1970, 1, 1, 0, 0, 0, 0)
                Dim t As Long = (ts.Ticks / 10000000)
                Return t.ToString()
            End Function

            'Original method actually returned a value that had decimal places associated with the timestamp 
            ':::Joseph Farrar 
            '------------------------ 
            'public virtual string GenerateTimeStamp() 
            '{ 
            ' TimeSpan ts = DateTime.UtcNow - new DateTime(1970, 1, 1, 0, 0, 0, 0); 
            ' return ts.TotalSeconds.ToString(); 
            '} 


            ''' <summary> 
            ''' Generate a nonce 
            ''' </summary> 
            ''' <returns></returns> 
            Public Overridable Function GenerateNonce() As String
                ' Just a simple implementation of a random number between 123400 and 9999999 
                Return random.[Next](123400, 9999999).ToString()
            End Function

        End Class
    End Namespace
End Namespace