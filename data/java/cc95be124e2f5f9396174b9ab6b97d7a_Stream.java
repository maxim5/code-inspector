/**
 * <copyright>
 * </copyright>
 *

 */
package de.grammarcraft.csflow.flow;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Stream</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link de.grammarcraft.csflow.flow.Stream#getLeftPort <em>Left Port</em>}</li>
 *   <li>{@link de.grammarcraft.csflow.flow.Stream#getMessage <em>Message</em>}</li>
 *   <li>{@link de.grammarcraft.csflow.flow.Stream#getRightPort <em>Right Port</em>}</li>
 * </ul>
 * </p>
 *
 * @see de.grammarcraft.csflow.flow.FlowPackage#getStream()
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
   * @see de.grammarcraft.csflow.flow.FlowPackage#getStream_LeftPort()
   * @model containment="true"
   * @generated
   */
  LeftPort getLeftPort();

  /**
   * Sets the value of the '{@link de.grammarcraft.csflow.flow.Stream#getLeftPort <em>Left Port</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Left Port</em>' containment reference.
   * @see #getLeftPort()
   * @generated
   */
  void setLeftPort(LeftPort value);

  /**
   * Returns the value of the '<em><b>Message</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Message</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Message</em>' attribute.
   * @see #setMessage(String)
   * @see de.grammarcraft.csflow.flow.FlowPackage#getStream_Message()
   * @model
   * @generated
   */
  String getMessage();

  /**
   * Sets the value of the '{@link de.grammarcraft.csflow.flow.Stream#getMessage <em>Message</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Message</em>' attribute.
   * @see #getMessage()
   * @generated
   */
  void setMessage(String value);

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
   * @see de.grammarcraft.csflow.flow.FlowPackage#getStream_RightPort()
   * @model containment="true"
   * @generated
   */
  RightPort getRightPort();

  /**
   * Sets the value of the '{@link de.grammarcraft.csflow.flow.Stream#getRightPort <em>Right Port</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Right Port</em>' containment reference.
   * @see #getRightPort()
   * @generated
   */
  void setRightPort(RightPort value);

} // Stream
