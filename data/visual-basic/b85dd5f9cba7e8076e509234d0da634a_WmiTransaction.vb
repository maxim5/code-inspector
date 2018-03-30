'******************************* File Header *********************************
'* File Name:    WmiTransaction.vb
'* Project:      WmiConnection
'* Copyright (c) Freddy Kuehne
'*
'* Default implementation for a wmi transaction.
'* 
'* This source is subject to the GNU Library General Public License (LGPL).
'* See http://wmiconnection.codeplex.com/license.
'* All other rights reserved.
'*****************************************************************************

''' <summary>
''' Default implementation for a wmi transaction.
''' </summary>
''' <remarks>
''' This class does currently nothing, because this is a readonly implementation.
''' </remarks>
Public Class WmiTransaction
  Inherits DbTransaction
#Region "Variables"
  ''' <summary>
  ''' Connection instance.
  ''' </summary>
  Private m_Connection As WmiConnection
#End Region

#Region "Constructors"
  ''' <summary>
  ''' Creates a new WmiTransaction.
  ''' </summary>
  ''' <param name="con">Connection-instance, where the transaction should be started</param>
  ''' <exception cref="ArgumentNullException">Exception occurs, when an empty <paramref name="con">connection parameter</paramref> is passed</exception>
  Friend Sub New(ByVal con As WmiConnection)
    If (con Is Nothing) Then Throw New ArgumentNullException("con")
    m_Connection = con
  End Sub
#End Region

#Region "IDbTransaction"
  ''' <summary>
  ''' <see cref="IDbTransaction.Commit"/>
  ''' </summary>
  ''' <remarks>
  ''' Empty Implementation.
  ''' </remarks>
  Public Overrides Sub Commit()
  End Sub

  ''' <summary>
  ''' Returns the associated connection.
  ''' </summary>
  ''' <returns>Associated connection</returns>
  Protected Overrides ReadOnly Property DbConnection As System.Data.Common.DbConnection
    Get
      DbConnection = m_Connection
    End Get
  End Property

  ''' <summary>
  ''' <see cref="IDbTransaction.IsolationLevel"/>
  ''' </summary>
  ''' <remarks>This implementation returns always <see cref="IsolationLevel.Unspecified"/></remarks>
  Public Overrides ReadOnly Property IsolationLevel As IsolationLevel
    Get
      IsolationLevel = IsolationLevel.Unspecified
    End Get
  End Property

  ''' <summary>
  ''' <see cref="IDbTransaction.Rollback"/>
  ''' </summary>
  ''' <remarks>Emptry Implementation</remarks>
  Public Overrides Sub Rollback()
  End Sub
#End Region
End Class
