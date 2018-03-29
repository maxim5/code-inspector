///<reference path='../tsWin/winjs.d.ts'/>
///<reference path='../tsWin/winrt.d.ts'/>
///<reference path='SortyCore.d.ts'/>

module SortyUI {
    export class Data {
        static instance: Data;

        list: any;  //WinJS.Binding.List;
        items: any;
        groupedItems: any;
        _group1: any;
        isLoaded: bool;
        progressElement: HTMLProgressElement;

        constructor () {
        }

        public init() {
            var self = this;
            this.isLoaded = false;
            this.list = new WinJS.Binding.List([]);

            this.groupedItems = this.list.createGrouped(
               function groupKeySelector(item) { return item.group.key; },
               function groupDataSelector(item) { return item.group; }
            );
            this.items = this.groupedItems;

            var group = this._group1 = { key: "Europe", title: "Europe", subtitle: "Europe Photos", backgroundImage: null, description: "" };

            this.itemProvider = new Sorty.Core.Providers.PhotohubProvider();
            this.itemProvider.getHashedItemsAsync("Europe").done(function (result) {
                result.forEach(function (item) {
                    self.list.push({
                        group: group,
                        title: item.name,
                        subtitle: "",
                        description: "",
                        content: "",
                        backgroundImage: URL.createObjectURL(item),
                        item: item
                    });
                });
                self.isLoaded = true;
                if (!!self.progressElement) {
                    self.progressElement.style.display = "none";
                }
            });

            this.groups = this.groupedItems.groups;
        }

        public groups: any;
        public itemProvider: Sorty.Core.Providers.PhotohubProvider;

        sortItems() {
            if(this.isLoaded == false) return;
            var self = this;
            this.list.sort(function (x, y) {
                return self.itemProvider.compare(x.item, y.item);
            });
            if (!!self.progressElement) {
                    self.progressElement.style.display = "none";
                }
            //this.list.notifyReload();
        }

        // Get a reference for an item, using the group key and item title as a
        // unique reference to the item that can be easily serialized.
        getItemReference(item) {
            return [item.group.key, item.title];
        }

        // This function returns a WinJS.Binding.List containing only the items
        // that belong to the provided group.
        getItemsFromGroup(group) {
            return this.items.createFiltered(function (item) { return item.group.key === group.key; });
        }

        // Get the unique group corresponding to the provided group key.
        resolveGroupReference(key) {
            for (var i = 0; i < this.groupedItems.groups.length; i++) {
                if (this.groupedItems.groups.getAt(i).key === key) {
                    return this.groupedItems.groups.getAt(i);
                }
            }
        }

        // Get a unique item from the provided string array, which should contain a
        // group key and an item title.
        resolveItemReference(reference) {
            for (var i = 0; i < this.groupedItems.length; i++) {
                var item = this.groupedItems.getAt(i);
                if (item.group.key === reference[0] && item.title === reference[1]) {
                    return item;
                }
            }
        }

        resolveItemReferenceByItem(referenceItem: any) {
            for (var i = 0; i < this.groupedItems.length; i++) {
                var item = this.groupedItems.getAt(i);
                if (item.item == referenceItem) {
                    return item;
                }
            }
        }
    }
}

SortyUI.Data.instance = new SortyUI.Data();
SortyUI.Data.instance.init();