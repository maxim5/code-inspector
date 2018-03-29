/**
 */
package org.eclipse.papyrus.RobotML;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Operation;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Algorithm</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * LibFileFormat can be elf, matlab, etc
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.papyrus.RobotML.Algorithm#getBase_Operation <em>Base Operation</em>}</li>
 *   <li>{@link org.eclipse.papyrus.RobotML.Algorithm#isIsExternal <em>Is External</em>}</li>
 *   <li>{@link org.eclipse.papyrus.RobotML.Algorithm#getExtFunctionName <em>Ext Function Name</em>}</li>
 *   <li>{@link org.eclipse.papyrus.RobotML.Algorithm#getLibPath <em>Lib Path</em>}</li>
 *   <li>{@link org.eclipse.papyrus.RobotML.Algorithm#getLibFileFormat <em>Lib File Format</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.papyrus.RobotML.RobotMLPackage#getAlgorithm()
 * @model
 * @generated
 */
public interface Algorithm extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Operation</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Operation</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Operation</em>' reference.
	 * @see #setBase_Operation(Operation)
	 * @see org.eclipse.papyrus.RobotML.RobotMLPackage#getAlgorithm_Base_Operation()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Operation getBase_Operation();

	/**
	 * Sets the value of the '{@link org.eclipse.papyrus.RobotML.Algorithm#getBase_Operation <em>Base Operation</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Operation</em>' reference.
	 * @see #getBase_Operation()
	 * @generated
	 */
	void setBase_Operation(Operation value);

	/**
	 * Returns the value of the '<em><b>Is External</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Is External</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is External</em>' attribute.
	 * @see #setIsExternal(boolean)
	 * @see org.eclipse.papyrus.RobotML.RobotMLPackage#getAlgorithm_IsExternal()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isIsExternal();

	/**
	 * Sets the value of the '{@link org.eclipse.papyrus.RobotML.Algorithm#isIsExternal <em>Is External</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is External</em>' attribute.
	 * @see #isIsExternal()
	 * @generated
	 */
	void setIsExternal(boolean value);

	/**
	 * Returns the value of the '<em><b>Ext Function Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ext Function Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ext Function Name</em>' attribute.
	 * @see #setExtFunctionName(String)
	 * @see org.eclipse.papyrus.RobotML.RobotMLPackage#getAlgorithm_ExtFunctionName()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getExtFunctionName();

	/**
	 * Sets the value of the '{@link org.eclipse.papyrus.RobotML.Algorithm#getExtFunctionName <em>Ext Function Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ext Function Name</em>' attribute.
	 * @see #getExtFunctionName()
	 * @generated
	 */
	void setExtFunctionName(String value);

	/**
	 * Returns the value of the '<em><b>Lib Path</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Lib Path</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Lib Path</em>' attribute.
	 * @see #setLibPath(String)
	 * @see org.eclipse.papyrus.RobotML.RobotMLPackage#getAlgorithm_LibPath()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getLibPath();

	/**
	 * Sets the value of the '{@link org.eclipse.papyrus.RobotML.Algorithm#getLibPath <em>Lib Path</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Lib Path</em>' attribute.
	 * @see #getLibPath()
	 * @generated
	 */
	void setLibPath(String value);

	/**
	 * Returns the value of the '<em><b>Lib File Format</b></em>' attribute.
	 * The default value is <code>"elf"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Lib File Format</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Lib File Format</em>' attribute.
	 * @see #setLibFileFormat(String)
	 * @see org.eclipse.papyrus.RobotML.RobotMLPackage#getAlgorithm_LibFileFormat()
	 * @model default="elf" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getLibFileFormat();

	/**
	 * Sets the value of the '{@link org.eclipse.papyrus.RobotML.Algorithm#getLibFileFormat <em>Lib File Format</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Lib File Format</em>' attribute.
	 * @see #getLibFileFormat()
	 * @generated
	 */
	void setLibFileFormat(String value);

} // Algorithm
