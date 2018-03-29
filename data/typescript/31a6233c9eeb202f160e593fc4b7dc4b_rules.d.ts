
interface ObjectFactory {
}

interface Rule extends Cloneable, CopyTo, Equals, HashCode, ToString {
	event: RuleEvent;
	actions: RuleAction[];
	name: string;
}

interface RuleAction extends Cloneable, CopyTo, Equals, HashCode, ToString {
	targetcomponent: string;
}

interface RuleActionClear extends RuleAction, Cloneable, CopyTo, Equals, HashCode, ToString {
	entity: string;
}

interface RuleActionEnable extends RuleAction, Cloneable, CopyTo, Equals, HashCode, ToString {
	invertable: boolean;
}

interface RuleActionRefreshValuelist extends RuleAction, Cloneable, CopyTo, Equals, HashCode, ToString {
	entity: string;
	parameterForSourcecomponent: string;
}

interface RuleActionReinitSubform extends RuleAction, Cloneable, CopyTo, Equals, HashCode, ToString {
	entity: string;
}

interface RuleActionTransferLookedupValue extends RuleAction, Cloneable, CopyTo, Equals, HashCode, ToString {
	sourcefield: string;
}

interface RuleEvent extends Cloneable, CopyTo, Equals, HashCode, ToString {
	type: string;
	entity: string;
	sourcecomponent: string;
}

interface Rules extends Cloneable, CopyTo, Equals, HashCode, ToString {
	rules: Rule[];
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
