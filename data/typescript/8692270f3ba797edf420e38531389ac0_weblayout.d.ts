
interface ObjectFactory {
}

interface WebAdvancedProperty extends Cloneable, CopyTo, Equals, HashCode, ToString {
	name: string;
	value: string;
}

interface WebButton extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	label: string;
	icon: string;
	disableDuringEdit: boolean;
}

interface WebButtonChangeState extends WebButton, Cloneable, CopyTo, Equals, HashCode, ToString {
	targetState: string;
}

interface WebButtonDummy extends WebButton, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebButtonExecuteRule extends WebButton, Cloneable, CopyTo, Equals, HashCode, ToString {
	rule: string;
}

interface WebButtonGenerateObject extends WebButton, Cloneable, CopyTo, Equals, HashCode, ToString {
	objectGenerator: string;
}

interface WebButtonHyperlink extends WebButton, Cloneable, CopyTo, Equals, HashCode, ToString {
	hyperlinkField: string;
}

interface WebCalcCell extends Cloneable, CopyTo, Equals, HashCode, ToString {
	components: WebComponent[];
	left: string;
	top: string;
	width: string;
	height: string;
}

interface WebCell extends Cloneable, CopyTo, Equals, HashCode, ToString {
	components: WebComponent[];
	colspan: number;
	rowspan: number;
}

interface WebCheckbox extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebCombobox extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebComponent extends Cloneable, CopyTo, Equals, HashCode, ToString {
	advancedProperties: WebAdvancedProperty[];
	id: string;
	name: string;
	column: number;
	row: number;
	colspan: number;
	rowspan: number;
	fontSize: string;
	textColor: string;
	bold: boolean;
	italic: boolean;
	underline: boolean;
}

interface WebContainer extends WebComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	grid: WebGrid;
	table: WebTable;
	calculated: WebGridCalculated;
	components: WebComponent[];
	opaque: boolean;
	backgroundColor: string;
}

interface WebDatechooser extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebEmail extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebFile extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebGrid extends Cloneable, CopyTo, Equals, HashCode, ToString {
	rows: WebRow[];
}

interface WebGridCalculated extends Cloneable, CopyTo, Equals, HashCode, ToString {
	cells: WebCalcCell[];
	minWidth: number;
	minHeight: number;
}

interface WebHtmlEditor extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebHyperlink extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebInputComponent extends WebComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	valuelistProvider: WebValuelistProvider;
	enabled: boolean;
	editable: boolean;
	columns: string;
	nextfocuscomponent: string;
	nextfocusfield: string;
}

interface WebLabel extends WebComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	forId: string;
}

interface WebLabelStatic extends WebComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	text: string;
}

interface WebLayout extends Cloneable, CopyTo, Equals, HashCode, ToString {
	grid: WebGrid;
	table: WebTable;
	calculated: WebGridCalculated;
}

interface WebListofvalues extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebMatrix extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	matrixColumns: WebMatrixColumn[];
	matrixPreferencesField: string;
	entityX: string;
	entityY: string;
	entityMatrix: string;
	entityFieldMatrixParent: string;
	entityFieldMatrixXRefField: string;
	entityMatrixValueField: string;
	entityMatrixNumberState: string;
	entityFieldCategorie: string;
	entityFieldX: string;
	entityYParentField: string;
	entityFieldY: string;
	entityXSortingFields: string;
	entityYSortingFields: string;
	entityXHeader: string;
	entityYHeader: string;
	entityMatrixReferenceField: string;
	entityMatrixValueType: string;
	entityXVlpId: string;
	entityXVlpIdfieldname: string;
	entityXVlpFieldname: string;
	entityXVlpReferenceParamName: string;
	cellInputType: string;
}

interface WebMatrixColumn extends Cloneable, CopyTo, Equals, HashCode, ToString {
	name: string;
	label: string;
	controltype: string;
	controltypeclass: string;
	enabled: string;
	notcloneable: string;
	insertable: string;
	rows: string;
	columns: string;
	resourceId: string;
	width: string;
	nextfocuscomponent: string;
	nextfocusfield: string;
}

interface WebPanel extends WebContainer, Cloneable, CopyTo, Equals, HashCode, ToString {
	title: string;
	borderWidth: string;
	borderColor: string;
}

interface WebPassword extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebPhonenumber extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebRow extends Cloneable, CopyTo, Equals, HashCode, ToString {
	cells: WebCell[];
}

interface WebSubform extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	subformColumns: WebSubformColumn[];
	controllerType: WebComponent;
	boAttrId: string;
	entity: string;
	autonumberSorting: boolean;
	notCloneable: boolean;
	multiEditable: boolean;
	toolbarOrientation: WebToolbarOrientation;
	opaque: boolean;
	uniqueMastercolumn: string;
	foreignkeyfieldToParent: string;
	parentSubform: string;
	dynamicCellHeightsDefault: boolean;
	ignoreSubLayout: boolean;
	newEnabled: boolean;
	editEnabled: boolean;
	deleteEnabled: boolean;
	cloneEnabled: boolean;
}

interface WebSubformColumn extends Cloneable, CopyTo, Equals, HashCode, ToString {
	controlType: WebComponent;
	valuelistProvider: WebValuelistProvider;
	name: string;
	label: string;
	controlTypeClass: string;
	enabled: boolean;
	notCloneable: boolean;
	insertable: boolean;
	rows: string;
	columns: string;
	resourceId: string;
	width: string;
	nextFocusComponent: string;
	nextFocusField: string;
	customUsageSearch: string;
	visible: boolean;
}

interface WebTab extends Cloneable, CopyTo, Equals, HashCode, ToString {
	title: string;
	content: WebComponent;
}

interface WebTabcontainer extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	tabs: WebTab[];
	selectedIndex: number;
}

interface WebTable extends Cloneable, CopyTo, Equals, HashCode, ToString {
	rows: WebRow[];
	columnSizes: number[];
	rowSizes: number[];
}

interface WebTextarea extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
	rows: number;
}

interface WebTextfield extends WebInputComponent, Cloneable, CopyTo, Equals, HashCode, ToString {
}

interface WebValuelistProvider extends Cloneable, CopyTo, Equals, HashCode, ToString {
	parameter: Parameter[];
	name: string;
	type: string;
	value: string;
	idFieldname: string;
	fieldname: string;
}

interface Parameter extends Cloneable, CopyTo, Equals, HashCode, ToString {
	name: string;
	value: string;
}

interface Cloneable {
}

interface CopyTo {
}

interface Equals {
}

interface HashCode {
}

interface ToString {
}

type WebButtonAction = 'DUMMY' | 'CHANGE_STATE' | 'EXECUTE_RULE' | 'GENERATE_OBJECT' | 'HYPERLINK';

type WebToolbarOrientation = 'HORIZONTAL' | 'VERTICAL' | 'HIDE';
