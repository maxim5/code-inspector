/*         Data Access Layer for Business Process Automation              */


#region NameSpace
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Data;
using System.Data.Sql;
using System.Data.SqlClient;
using ebird.QueryGenerics;
#endregion
namespace ebird.BusinessProcessAutomationDAS
{
    #region Structure
    public struct StockDetails
    {

        string productId;

        public string ProductID
        {
            get { return productId; }
            set { productId = value; }
        }

        string productName;

        public string ProductName
        {
            get { return productName; }
            set { productName = value; }
        }

        string lotId;

        public string LotID
        {
            get { return lotId; }
            set { lotId = value; }
        }

    }
    #endregion

    public class StockManagementDAS
    {
        #region Fiellds
        ebird.QueryGenerics.QueryGenerics queryGenerics;
        #endregion

        #region Constructor
        public StockManagementDAS(string connectionString)
        {
            queryGenerics = new ebird.QueryGenerics.QueryGenerics(connectionString);
        }
        #endregion


        #region Methods

        #region "ProductHistory.aspx"
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public DataSet GetBatchHistory()
        {
            #region Fields
            string query = string.Empty;
            DataSet batchHistoryDS;
            #endregion

            batchHistoryDS = new DataSet();
            query = "SELECT     BatchHistory.LotID, dbo.FormatDateTime(BatchHistory.DateTime,'LONGDATEANDTIME') as DateTime,isnull(UserDetails.UD_FirstName,'')+' '+isnull(UserDetails.UD_MiddleName,' ')+' '+isnull(UserDetails.UD_LastName,' ') as EmployeeName , " +
                    " BatchHistory.Direction, BatchHistory.LotName, Direction.DirectionName " +
                    " FROM BatchHistory INNER JOIN UserDetails ON BatchHistory.MovedBy = UserDetails.UD_UserPersonalID INNER JOIN " +
                    " Direction ON BatchHistory.Direction = Direction.DirectionId order by BatchHistory.DateTime desc";
            batchHistoryDS = queryGenerics.ExecuteQuery(query);//commonClass.ExecuteQuery(query);
            return batchHistoryDS;
        }

        /// <summary>
        /// To get Producthistory
        /// </summary>
        /// <param name="stockData"></param>
        /// <returns></returns>
        public DataSet GetProductHistory(StockDetails stockData)
        {
            #region Fields
       
            string query = string.Empty;
            DataSet productHistoryDS;
            #endregion

            productHistoryDS = new DataSet();
            query = "SELECT     ProductList.ProductName, BatchHistory.LotName, BatchHistory.Direction, BatchHistory.DateTime, History.Quantity, BatchHistory.LotID, " +
                    " Direction.DirectionName FROM  BatchHistory INNER JOIN  History ON BatchHistory.LotID = History.LotID INNER JOIN " +
                    " ProductList ON History.ItemID = ProductList.ProductID INNER JOIN  Direction ON BatchHistory.Direction = Direction.DirectionId where BatchHistory.LotID='" + stockData.LotID + "'";
            productHistoryDS = queryGenerics.ExecuteQuery(query);
            return productHistoryDS;

        }
        #endregion

        #region "StockStatus.aspx"
        public DataSet GetProdustStockHistory()
        {
            #region Fields
            string query = string.Empty;
            #endregion
            query = "SELECT     ProductList.ProductID, ProductList.ProductCode, ProductList.ProductName, ProductList.ProductDescription, ProductList.ListPrice, " +
                      "ProductStock.Quantity, dbo.FormatDateTime(ProductStock.LastUpdated,'SHORTDATE') as LastUpdated, ProductType.TypeName, ProductList.Unit, ProductList.Specification " +
                       " FROM         ProductList INNER JOIN  ProductStock ON ProductList.ProductID = ProductStock.ProductID INNER JOIN " +
                      " ProductType ON ProductList.Type = ProductType.Type order by ProductStock.LastUpdated desc";
            DataSet stockStatus = queryGenerics.ExecuteQuery(query);
            return stockStatus;
        }
        #endregion
        #endregion
    }
}
