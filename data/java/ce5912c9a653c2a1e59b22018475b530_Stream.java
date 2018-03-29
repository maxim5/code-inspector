/**
 * <copyright>
 * </copyright>
 *

 */
package de.grammarcraft.flow.flow;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Stream</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link de.grammarcraft.flow.flow.Stream#getLeftPort <em>Left Port</em>}</li>
 *   <li>{@link de.grammarcraft.flow.flow.Stream#getMessage <em>Message</em>}</li>
 *   <li>{@link de.grammarcraft.flow.flow.Stream#getRightPort <em>Right Port</em>}</li>
 * </ul>
 * </p>
 *
 * @see de.grammarcraft.flow.flow.FlowPackage#getStream()
 * @model
 * @generated
 */
public interface Stream extends EObject
{
  /**
   * Returns the value of the '<em><b>Left Port</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Left Port</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Left Port</em>' containment reference.
   * @see #setLeftPort(LeftPort)
   * @see de.grammarcraft.flow.flow.FlowPackage#getStream_LeftPort()
   * @model containment="true"
   * @generated
   */
  LeftPort getLeftPort();

  /**
   * Sets the value of the '{@link de.grammarcraft.flow.flow.Stream#getLeftPort <em>Left Port</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Left Port</em>' containment reference.
   * @see #getLeftPort()
   * @generated
   */
  void setLeftPort(LeftPort value);

  /**
   * Returns the value of the '<em><b>Message</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Message</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Message</em>' containment reference.
   * @see #setMessage(JvmTypeReference)
   * @see de.grammarcraft.flow.flow.FlowPackage#getStream_Message()
   * @model containment="true"
   * @generated
   */
  JvmTypeReference getMessage();

  /**
   * Sets the value of the '{@link de.grammarcraft.flow.flow.Stream#getMessage <em>Message</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Message</em>' containment reference.
   * @see #getMessage()
   * @generated
   */
  void setMessage(JvmTypeReference value);

  /**
   * Returns the value of the '<em><b>Right Port</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Right Port</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Right Port</em>' containment reference.
   * @see #setRightPort(RightPort)
   * @see de.grammarcraft.flow.flow.FlowPackage#getStream_RightPort()
   * @model containment="true"
   * @generated
   */
  RightPort getRightPort();

  /**
   * Sets the value of the '{@link de.grammarcraft.flow.flow.Stream#getRightPort <em>Right Port</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Right Port</em>' containment reference.
   * @see #getRightPort()
   * @generated
   */
  void setRightPort(RightPort value);

} // Stream
