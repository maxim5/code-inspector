import { Injectable } from '@angular/core';
import { Headers, Response, URLSearchParams } from '@angular/http';
import { Observable } from 'rxjs';
import { WsTab } from '../../explorertrees/explorertrees.model';
import { Logger } from '../../log/shared/logger';
import {
	ColumnAttribute,
	Preference,
	SearchtemplateAttribute,
	SearchtemplatePreferenceContent,
	SideviewmenuPreferenceContent
} from '../../preferences/preferences.model';
import { SearchtemplateService } from '../../searchtemplate/searchtemplate.service';
import { FqnService } from '../../shared/fqn.service';
import { NuclosConfigService } from '../../shared/nuclos-config.service';
import { NuclosHttpService } from '../../shared/nuclos-http.service';
import { BoAttr, BoViewModel, EntityAttrMeta, EntityMeta, EntityMetaData, InputType } from './bo-view.model';
import { EntityObject } from './entity-object.class';
import { NuclosI18nService } from '../../i18n/shared/nuclos-i18n.service';
import { DateFormatter } from '@angular/common/src/pipes/intl';
import { DatetimeService } from '../../shared/datetime.service';

/**
 * TODO: Split this into more meaningful separate services, instead of one monolith for everything.
 */
@Injectable()
export class DataService {

	constructor(
		private http: NuclosHttpService,
		private nuclosConfig: NuclosConfigService,
		private fqnService: FqnService,
		private searchtemplateService: SearchtemplateService,
		private i18n: NuclosI18nService,
		private dateTimeService: DatetimeService,
		private $log: Logger
	) {
	}


	loadList(
		meta: EntityMeta,
		sideviewmenuPreference?: Preference<SideviewmenuPreferenceContent>,
		searchtemplatePreference?: Preference<SearchtemplatePreferenceContent>,
		offset = 0,
		chunkSize = 40,
		getTotal = true
	): Observable<BoViewModel> {


		let columns: (ColumnAttribute|undefined)[] = sideviewmenuPreference ? sideviewmenuPreference.content.columns.filter(c => c.selected) : [];
		let sort = '';
		if (sideviewmenuPreference && sideviewmenuPreference.content.columns) {
			sort = this.getSortString(sideviewmenuPreference.content.columns);
		}

		let searchInputText = this.searchtemplateService.getCurrentSearchInputText();

		let searchFilter = undefined;

		// add attributes for title and info
		let attributeFqns: (string|undefined)[] = columns.map(a => a ? a.boAttrId : undefined);
		let titleAttributeFqns: string[] = this.fqnService.parseFqns(meta.getTitlePattern());
		let infoAttributeFqns: string[] = this.fqnService.parseFqns(meta.getInfoPattern());

		columns = Array
			.from(new Set<string|undefined>([...attributeFqns, ...titleAttributeFqns, ...infoAttributeFqns]))

			// filter out columns from other EO's (at the moment only supported by the Java-client result list)
			.filter(fqn => fqn && fqn.startsWith(meta.getBoMetaId() + '_'))

			.map(fqn => {
					if (fqn) {
						let attr = meta.getAttributeMetaByFqn(fqn);
						if (attr) {
							return {
								boAttrId: attr.getAttributeID(),
							} as ColumnAttribute
						}
					}
					return undefined;
				}
			)
			.filter(fqn => fqn !== undefined);

		let filter = this.buildFilter(meta, searchInputText, searchFilter, columns, offset, chunkSize, sort);

		let queryObject = {
			where: ''
		};

		if (searchtemplatePreference && searchtemplatePreference.content.columns) {
			queryObject.where = this.buildWhereCondition(searchtemplatePreference.content.columns, meta);
		}

		return this.http.post(
			this.nuclosConfig.getRestHost() + '/bos/' + meta.getBoMetaId() + '/query?offset=' + offset + '&gettotal=' + getTotal,
			queryObject,
			{search: filter}
		).map((response: Response) => {
			let result: BoViewModel = response.json();
			let entityObjects: EntityObject[] = [];
			for (let bo of result.bos) {
				entityObjects.push(new EntityObject(<any>bo));
			}
			result.bos = entityObjects;
			return result;
		});
	}


	/**
	 * Builds the 'AND'-concatenated WHERE condition based on the given search object.
	 *
	 * TODO: searchObject type
	 *
	 * @param searchObject
	 * @param meta
	 * @returns {any}
	 */
	buildWhereCondition(attributes: SearchtemplateAttribute[], meta: EntityMeta): string {
		let result = {
			aliases: {}
		} as any;
		let index = 0;
		let andArray: string[] = [];

		// // TODO: Get rid of this workaround
		// searchObject = this.synchronizeAttributes(searchObject);

		for (let searchElement of attributes) {

			if (!searchElement.operator || searchElement.enableSearch === false) {
				continue;
			}

			let attribute = this.fqnService.getAttributeByFqn(meta, searchElement.boAttrId);
			attribute.inputType = this.getInputType(attribute);
			let fqn = searchElement.boAttrId;
			let alias = 'col' + index;
			let useTicks = this.needsTicks(attribute, searchElement.operator);
			let andPart = alias + ' ' + searchElement.operator + ' ';

			if (!this.isUnaryOperator(searchElement.operator)) {
				let value = this.formatSearchTemplateValue(searchElement.value, attribute, useTicks);
				if (value === null) {
					this.$log.warn('No value given for non-unary operator \'%o\' on attribute \'%o\', ignoring', searchElement.operator, attribute);
					continue;
				}

				andPart += value;
			}

			andArray.push(andPart);
			result.aliases[alias] = {boAttrId: fqn};
			index++;
		}

		result.clause = andArray.join(' AND ');

		return result;
	}

	getInputType(attribute): string {
		if (attribute.reference) {
			return InputType.REFERENCE;
		} else if (attribute.type === 'Decimal' || attribute.type === 'Integer') {
			return InputType.NUMBER;
		} else if (attribute.type === 'String') {
			return InputType.STRING;
		} else if (attribute.type === 'Date' || attribute.type === 'Timestamp') {
			return InputType.DATE;
		} else if (attribute.type === 'Boolean') {
			return InputType.BOOLEAN;
		}
		return InputType.STRING;
	}

	/**
	 * Determines if the ticks should be used to escape the value for the given attribute in a WHERE-query.
	 *
	 * @param attribute
	 * @param operator
	 * @returns {boolean}
	 */
	private needsTicks(attribute: any, operator: string): boolean {
		if (attribute.system) {
			return true;
		} else if (attribute.inputType === InputType.NUMBER) {
			return false;
		} else if (attribute.inputType === InputType.REFERENCE && operator === '=') {
			return false;
		}

		return true;
	}

	/**
	 * Validates and formats the search input value according to the "input type" of the attribute.
	 *
	 * @param searchElement
	 * @param attribute
	 * @param useTicks
	 * @returns {any}
	 */
	private formatSearchTemplateValue(
		value: any,
		attribute: BoAttr,
		useTicks: boolean
	): string {

		// if (attribute.inputType === 'reference') {
		if (attribute.reference) {
			value = (value && value.id) ? value.id : value;
		}

		if (value !== undefined
			&& value.length === undefined
			&& (
				attribute.inputType === InputType.NUMBER
				|| attribute.inputType === InputType.REFERENCE
				|| attribute.inputType === InputType.DATE
			)
			|| value !== undefined && value.length > 0) {

			if (useTicks) {
				if (attribute.inputType === InputType.DATE) {
					value = moment(value).format('YYYY-MM-DD');
				}
				if (typeof value !== 'number') {
					value = value.replace(/'/g, ''); // remove '
				}
			}
			if (useTicks) {
				value = '\'' + value + '\'';
			}
		} else {
			value = null;
		}

		return value;
	}

	public formatAttribute(value, attrMeta: EntityAttrMeta): string {
		let result = value;
		if (value.name !== undefined) {
			result = value.name !== null ? value.name : '';
		} else if (attrMeta.isBoolean()) {
			result = value ? this.i18n.getI18n('common.yes') : this.i18n.getI18n('common.no');
		} else if (attrMeta.isDate()) {
			result = this.dateTimeService.formatDate(value);
		} else if (attrMeta.isTimestamp()) {
			result = this.dateTimeService.formatTimestamp(value);
		}
		return result;
	}

	/**
	 * Determines if the given operator is unary.
	 *
	 * TODO: Available operators and their types should be defined somewhere centrally.
	 *
	 * @param operator
	 * @returns {boolean}
	 */
	private isUnaryOperator(operator: string): boolean {
		return operator === 'is null' || operator === 'is not null';
	}

	/**
	 * TODO
	 *    - subform column selection
	 *    - searchfilter
	 *    - search (text)
	 *
	 * @param meta
	 * @param sideviewmenuPreference
	 * @param format
	 * @param pageOrientationLandscape
	 * @param isColumnScaled
	 * @returns {Observable<string>}
	 */
	exportBoList(
		boId: number | undefined,
		refAttrId: string | undefined,
		meta: EntityMeta,
		sideviewmenuPreference: Preference<SideviewmenuPreferenceContent>,
		format: string,
		pageOrientationLandscape: boolean,
		isColumnScaled: boolean
	): Observable<string> {

		let isSubform: boolean = boId !== undefined && refAttrId !== undefined;

		let offset = undefined;
		let chunkSize = undefined;

		let columns: ColumnAttribute[] = [];
		if (!isSubform) {
			columns = sideviewmenuPreference ? sideviewmenuPreference.content.columns : [];
		}

		let sort = '';
		if (sideviewmenuPreference && sideviewmenuPreference.content.columns) {
			sort = this.getSortString(sideviewmenuPreference.content.columns);
		}

		let filter = {
			offset: offset,
			chunksize: chunkSize,
			gettotal: false,
			countTotal: true,
			search: undefined, // TODO
			searchFilter: undefined, // TODO
			withTitleAndInfo: false,
			attributes: '',
			sort: '',
			searchCondition: ''
		};
		// request only given attributes
		filter.attributes =
			columns
			// use short name instead of fqn to avoid problems with to long query param
				.map(attribute => this.fqnService.getShortAttributeName(meta.getBoMetaId(), attribute.boAttrId))
				.join(',');

		filter.sort = this.getSortString(columns);

		let searchCondition = this.getSearchCondition(meta);
		if (searchCondition) {
			filter.searchCondition = searchCondition;
		}


		let headers = new Headers();
		headers.append('Content-Type', 'application/json');

		let url;
		if (!isSubform) {
			url = this.nuclosConfig.getRestHost()
				+ '/bos/' + meta.getBoMetaId()
				+ '/boListExport/' + format + '/' + pageOrientationLandscape + '/' + isColumnScaled;
		} else {
			url = this.nuclosConfig.getRestHost()
				+ '/bos/' + meta.getBoMetaId() + '/'
				+ boId
				+ '/subBos/'
				+ refAttrId
				+ '/export/' + format + '/' + pageOrientationLandscape + '/' + isColumnScaled;
		}

		if (filter.attributes.length > 0) {
			url += 'attributes=' + filter.attributes;
		}
		url
			+= '&countTotal=true&gettotal=false&offset=0&search='
			+ '&sort=' + filter.sort
			+ '&withTitleAndInfo=false';

		return (
			isSubform
				?
				this.http.get(
					url,
					{
						headers: headers
					}
				)
				:
				this.http.post(
					url,
					filter,
					{
						headers: headers
					}
				))
			.map((response: Response) => response.json())
			.map(data => data['links'].export.href);
	}

	private getSortString(columns: ColumnAttribute[]) {
		return columns
			.filter(col => col.sort && col.sort.direction)
			.sort(
				(a, b) => {
					if (!a.sort || !b.sort || a.sort.prio === undefined || b.sort.prio === undefined) {
						return 0;
					}
					if (a.sort.prio < b.sort.prio) {
						return -1;
					}
					if (a.sort.prio > b.sort.prio) {
						return 1;
					}
					return 0;
				}
			)
			.map(column => column.boAttrId + (column.sort ? '+' + column.sort.direction : ''))
			.join(',');
	}


	/**
	 * TODO: Rename 'BO' to 'entity object', 'BO meta' to 'entity class'.
	 *
	 * @param boMetaId
	 * @param boId
	 * @returns {Observable<R>}
	 */
	getBo(boMetaId: string, boId: number): Observable<EntityObject> {
		return this.http.get(
			this.nuclosConfig.getRestHost() + '/bos/' + boMetaId + '/' + boId
		)
			.map((response: Response) => new EntityObject(response.json()));
	}

	/**
	 * Fetches the corresponding EO for the given attribute (with only this one attribute filled).
	 * attributeId must be a fully quallified name!
	 *
	 * @param sourceAttributeId
	 * @returns {Observable<R>}
	 */
	fetchByAttribute(
		sourceAttributeId: string,
		sourceEOId: number,
		targetAttributeId: string,
		targetEOId?: number
	): Observable<EntityObject> {
		// TODO: Splitting by underscore is potentially unsafe, the FQN format is flawed.
		let index = sourceAttributeId.lastIndexOf('_');
		if (index <= 0) {
			throw 'Attribute ID must be a fully qualified name! Illegal value: ' + sourceAttributeId;
		}
		let url = this.nuclosConfig.getRestHost() + '/data/fieldget/'
			+ sourceAttributeId + '/'
			+ sourceEOId + '/'
			+ targetAttributeId + '/'
			+ targetEOId;

		return this.http.get(url)
			.map((response: Response) => new EntityObject(response.json()));
	}


	private buildFilter(
		meta: EntityMeta,
		search: string,
		searchFilter: string | undefined,
		attributes,
		offset: number,
		chunkSize: number,
		sort
	): URLSearchParams {

		let searchParams: URLSearchParams = new URLSearchParams();
		if (chunkSize) {
			searchParams.append('chunksize', '' + chunkSize);
		}
		searchParams.append('countTotal', 'true');
		searchParams.append('search', search);
		if (searchFilter) {
			searchParams.append('searchFilter', searchFilter);
		}
		searchParams.append('withTitleAndInfo', 'false');


		if (attributes === undefined) {
			attributes = [];
		}

		// request only given attributes
		if (attributes !== undefined) {
			searchParams.append('attributes',
				attributes
				// use short name instead of fqn to avoid problems with to long query param
					.map(attribute => this.fqnService.getShortAttributeName(meta.getBoMetaId(), attribute.boAttrId))
					.join(',')
			);
		}

		if (sort !== undefined) {
			searchParams.append('sort', sort);
		}

		let searchCondition = this.getSearchCondition(meta);
		if (searchCondition) {
			searchParams.append('searchCondition', searchCondition);
		}
		return searchParams;
	}


	private getSearchCondition(metaData: EntityMeta) {
		if (!metaData || !metaData.getAttributes()) {
			return undefined;
		}

		let hasFieldFilter = false;
		let fieldFilter = 'CompositeCondition:AND:[';

		for (let attrKey of Object.keys(metaData.getAttributes())) {
			let attribute = <any>metaData.getAttribute(attrKey);

			if (!attribute.ticked) {
				continue;
			}

			let fieldName = this.fqnService.getShortAttributeName(metaData.getBoMetaId(), attribute.boAttrId);
			if (attribute.search) {
				if (hasFieldFilter) {
					fieldFilter += ',';
				}

				fieldFilter += 'LikeCondition:LIKE:';
				fieldFilter += fieldName;
				fieldFilter += ':*' + attribute.search + '*';

				hasFieldFilter = true;
			}

			if (attribute.valuelist && attribute.valuelist.length > 0) {
				let hasInCondition = false;
				let inCondition = '';

				for (let key of Object.keys(attribute.valuelist)) {
					let value = attribute.valuelist[key];
					if (value.ticked && value.name) {
						if (hasInCondition) {
							inCondition += ',';
						}

						// NUCLOS-4111 CollectableInCondition works with the String Name of a Reference Field
						// TODO: It would be better if it worked with the IDs
						inCondition += '\'' + value.name + '\'';
						hasInCondition = true;
					}
				}

				if (hasInCondition) {
					if (hasFieldFilter) {
						fieldFilter += ',';
					}

					fieldFilter += 'InCondition:IN:';
					fieldFilter += fieldName;
					fieldFilter += ':[';
					fieldFilter += inCondition;
					fieldFilter += ']';

					hasFieldFilter = true;
				}
			}
		}

		if (hasFieldFilter) {
			return fieldFilter + ']';
		}

		return undefined;
	}

	loadMatrixData(webMatrix: WebMatrix, eo: EntityObject): Observable<any> {
		let link = this.nuclosConfig.getRestHost() + '/data/data/matrix/';

		let postData: any = webMatrix;
		postData.entity = eo.getEntityClassId();
		postData.boId = eo.getId();

		return this.http.post(link, JSON.stringify(postData)
		).map((response: Response) => response.json());
	}

	loadTreeData(tree: WsTab): Observable<any[]> {
		let link = this.nuclosConfig.getRestHost() + '/data/tree/' + tree.boMetaId + '/' + tree.boId;

		return this.http.get(link).map((response: Response) => response.json());
	}

	loadSubTreeData(node_id: string): Observable<any[]> {
		let link = this.nuclosConfig.getRestHost() + '/data/subtree/' + node_id;

		return this.http.get(link).map((response: Response) => response.json());
	}

}
