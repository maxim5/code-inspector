
interface WorkItemSimple {
    ID: number;
    Name: string;
    ProjectID: number;
    ProjectName: string;
}

interface BugsItem extends WorkItemSimple {
    Actives: number;
    Totals: number;
}

interface KeyDataIndicators {
    BugsCount: number;
    CSSBugs: string;
    CriticalItemsCount: number;
    CSSCriticalItems: string;
    UserstoriesCountDown: number;
    CSSUserstoriesCountDown: string;
}

interface KPIBugs {
    kpi: KeyDataIndicators;
    BugList: BugsItem[];
    ReleaseBugList: BugsItem[];
}


interface KanbanBugsItem {
    NewBugs: number[];
    FixedBugs: number[];
}

interface KPIKanbanBugs {
    kpi: KeyDataIndicators;
    BugItems: KanbanBugsItem;
}