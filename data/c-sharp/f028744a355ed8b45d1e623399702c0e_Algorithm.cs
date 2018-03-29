/**             Š 2008 Avanade Inc. All Rights Reserved.
 * 
 * Authors:     Joris Valkonet joris.valkonet@avanade.com  Avanade Netherlands
 *              Thanh Luc      thanh.luc@avanade.com       Avanade Netherlands
 *              Mark Beerens   mark.beerens@avanade.com    Avanade Netherlands
 * 
 * Content:     This class is the implementation of the AlgorithmBase class.
 *              The following functionalities are implemented in this class:
 *                  - Reading and transformation of the trainings data
 *                  - Calling the SMO algorithm
 *                  - Persistance of the outcomes of the SMO algorithm
 *                  - Prediction of new cases
 *                  - Some mining functions
 *                 
 * */
#region [Using]
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.SqlServer.DataMining.PluginAlgorithms;
using Avanade.Datamining.SMO;
//using System.Diagnostics;
#endregion

namespace Avanade.Datamining
{

    public class Algorithm : AlgorithmBase
    {
        #region [Members]
        // Mining parameters
        protected MiningParameterCollection algorithmParams;
        // trace notifications during processing
        public TaskProgressNotification trainingProgress;
        //the target attribute
        public uint TargetAttribute;
        //classifiers
        public SMO.SMO[][] classifiers;
        //the individuals
        private Instances[] instances;

        //the maximum number of individuals for training
        public int maximumInput = 255;
        //the tradeoff parameter between margin and misclassification error
        private double C = 1;
        //the size of the cached kernel
        private int cacheSize = 25007;

        //the size of the attributes
        public int attributeSize = 0;

        // The Kernel type and the parameters of the kernel
        private string strKernelType = "";

        //the gamma parameter for the RBF kernel
        private double gamma = 0.2;
        //the lower order of the polynomial kernel
        private bool lowerOrder = false;
        //the exponent of the polynomial kernel
        private double exponent = 2;
        #endregion

        #region [Constructor and Training]

        /// <summary>
        /// The constructor of the algorithm..  
        /// </summary>
        public Algorithm()
        {
        }

        /// <summary>
        /// No initialization ... yet
        /// </summary>
        protected override void Initialize()
        {
        }

        /// <summary>
        /// parsing the default parameters
        ///  - MAXIMUM_INPUT, the maximum number of individuals used for training
        ///  - C, the tradeoff between margin and misclassification
        ///  - CACHE_SIZE, the size of the cache
        /// </summary>
        /// <param name="trainingParams"></param>
        protected void parseParameters(MiningParameterCollection trainingParams)
        {
            
            //parse the parameters
            foreach (MiningParameter param in trainingParams)
            {
                //if the param is a param
                if (param.Name != null)
                {
                    String paramName = param.Name.ToLower();
                    //set the value correct
                    switch (paramName)
                    {
                        
                        //the sensitivity of the algorithm
                        case "c":
                            C = System.Convert.ToDouble(param.Value);
                            break;
                        //the cashe size of the kernel cache
                        case "cache_size":
                            cacheSize = System.Convert.ToInt32(param.Value);
                            break;
                        //the kernel type
                        case "kernel_type":
                            strKernelType = (string) param.Value;
                            strKernelType = strKernelType.ToLower();        // convert to lower characters
                            break;
                        //the parameters of the specified kernel
                        case "exponent":
                            try
                            {
                                exponent = Convert.ToDouble(param.Value);
                            }
                            catch
                            {
                                throw new Exception("Converting the [Exponent] parameter to a double failed");
                            }
                            break;
                        case "gamma":
                            try
                            {
                                gamma = Convert.ToDouble(param.Value);
                            }
                            catch
                            {
                                throw new Exception("Converting the [gamma] parameter to a double failed");
                            }
                            break;
                        case "homogeneous":
                            try
                            {
                                if (param.Value.ToString().ToLower() == "true")
                                {
                                    lowerOrder = true;
                                }
                                else
                                {
                                    lowerOrder = false;
                                }
                            }
                            catch
                            {
                                throw new Exception("Converting the [homogeneous] parameter to a bool failed");
                            }
                            break;
                    }
                }
            }

        }


        /// <summary>
        /// the real training algorithm
        ///  outline:
        ///  1. parse the parameters
        ///  2. determine random which individuals are being used for training
        ///  3. reading the individuals and storing them into instances[]
        ///  4. learning all the classifiers; 1 vs 1 learning method
        ///     for each possible predict value, learn a classifier against all other
        ///     possiblie predict value
        /// </summary>
        /// <param name="caseSet">The set of trainings examples</param>
        /// <param name="trainingParams">The parameters added to the algorithm</param>
        protected override void InsertCases(PushCaseSet caseSet, MiningParameterCollection trainingParams)
        {
            //parse the parameters
            parseParameters(trainingParams);
            //determine the target attribute
            TargetAttribute = getTargetAttribute();
            
            //read the trainings examples and store them in the local cache
            MyCaseProcessor processor = new MyCaseProcessor(this);
            caseSet.StartCases(processor);
            Instances[] instances = processor.instances; //the local caches.. order by class

            //determine the individuals that will be used for learning.
            if (maximumInput != 0 && (int)MarginalStats.GetTotalCasesCount() - maximumInput > 0)
            {
                //filter the classes with no examples
                int[] scatter = new int[instances.Length];
                int zeros = 0;
                for (int i = 0; i < scatter.Length; i++)
                {
                    scatter[i] = instances[i].instances.Length;
                    if (scatter[i] == 0)
                    {
                        zeros++;
                    }
                }

                //the average amount of examples per class
                int average = (int)(maximumInput / (instances.Length - zeros));
                //determine whether all examples are used for learning
                bool[] fullUse = new bool[instances.Length];

                //the examples that will be fed to the SMO algorithm
                Instances[] newInstances = new Instances[instances.Length];
                
                //the total number of trianing examples
                int total = 0;

                //for all different classes.. apply binary learning between all classes
                for (int i = 0; i < newInstances.Length; i++)
                {
                    //create the trainingset
                    newInstances[i] = new Instances(new Instance[] { }, instances[i].labels, instances[i].attributeSize);
                    //randomize the examples
                    instances[i].randomizeInstances(500);
                    
                    for (int j = 0; j < average && j < instances[i].instances.Length; j++)
                    {
                        newInstances[i].addInstance(instances[i].instances[j]);
                        total++;
                    }
                    if (instances[i].instances.Length > average)
                    {
                        fullUse[i] = false;
                    }
                    else
                    {
                        fullUse[i] = true;
                    }
                }
                instances = newInstances;

            }


            this.instances = instances;
            //determing the number of labels and 
            //create a dataset for each label
            uint numberOfLabels = AttributeSet.GetAttributeStateCount(getTargetAttribute());
            int[] labels = getLabels();
            
            //for all combinations of labels
            //learn the classifier
            classifiers = new SMO.SMO[numberOfLabels][];
            for (int i = 0; i < numberOfLabels; i++)
            {
                //create a new instance of the SMO algorithm
                classifiers[i] = new SMO.SMO[numberOfLabels];
                //apply the binary learning
                for (int j = i + 1; j < numberOfLabels; j++)
                {
                    //construct the data from the datasets and randomize their order
                    Instances data = new Instances(new Instance[] { }, labels, instances[i].attributeSize);

                    int a = 0;
                    foreach (Instance instance in instances[i].instances)
                    {
                        data.addInstance(instance);
                        a++;
                    }
                    int b = 0;
                    foreach (Instance instance in instances[j].instances)
                    {
                        data.addInstance(instance);
                        b++;
                    }
                    data.randomizeInstances(new Random().Next(500));

                    data.label = j;


                    //create the kernel                                      
                    Kernel kernel = getKernel(data);
                    if (kernel == null)
                    {
                        throw new ApplicationException("Kernel not found");
                    }

                    //create and learn the classifier
                    classifiers[i][j] = new SMO.SMO();
                    classifiers[i][j].C = C;
                    classifiers[i][j].buildClassifier(data, i, j, kernel);
                }
            }
        }

        /// <summary>
        /// Convert the input case to continuous space. This consists of 2 steps:
        /// 1. Convert all discrete columns to continuous input space
        /// 2. Scale the input columns to lie between 0 and 1
        /// </summary>
        /// <param name="inputCase">An input example</param>
        /// <param name="doubleValues">the output in continous n-dimensional space</param>
        /// <param name="label">the label of the example</param>
        /// <param name="attributeSize">the size of the n-dimensional space</param>
        public void getValues(MiningCase inputCase, out double[] doubleValues, out uint label, out int attributeSize)
        {
            //save the input example in a dictionary
            SortedDictionary<uint, double> dict = new SortedDictionary<uint, double>();
            bool bContinue = inputCase.MoveFirst();
            while (bContinue)
            {
                if (inputCase.Value.IsDouble)
                {
                    dict.Add(inputCase.Attribute, inputCase.DoubleValue);
                }
                else
                {
                    dict.Add(inputCase.Attribute, inputCase.Value.Index);
                }
                bContinue = inputCase.MoveNext();
            }

            //the linked list will save all the values
            LinkedList<double> values = new LinkedList<double>();
            label = 0;
            attributeSize = 0;

            //loop through the dictionary and scale the input and store it in the linked list
            SortedDictionary<uint, double>.Enumerator enumerator = dict.GetEnumerator();
            while (enumerator.MoveNext())
            {
                uint attribute = enumerator.Current.Key;
                double value = enumerator.Current.Value;
                if (!isNominal(attribute))
                {
                    //scaling
                    double max = MarginalStats.GetAttributeStats(attribute).Max;
                    double min = MarginalStats.GetAttributeStats(attribute).Min;
                    value = (value - min) / (max - min);
                    if (Double.IsNaN(value) || Double.IsInfinity(value))
                    {
                        value = 0;
                        attributeSize++;
                    }
                    values.AddLast(value);
                }
                else
                {
                    if (isTarget(attribute))
                    {
                        label = (uint)value;
                    }
                    else
                    {
                        for (uint i = 0; i < AttributeSet.GetAttributeStateCount(attribute); i++)
                        {
                            if (i == (uint)value)
                            {
                                values.AddLast(1);
                            }
                            else
                            {
                                values.AddLast(0);
                            }
                            attributeSize++;
                        }
                    }
                }
            }
            this.attributeSize = attributeSize;
            doubleValues = new double[values.Count];
            values.CopyTo(doubleValues, 0);
        }

        #endregion

        #region Persistance
        /// <summary>
        /// Persistance enumeration
        /// </summary>
        enum Persistent
        {
            Parameters, Classifiers
        }


        /// <summary>
        /// Load an already trained model
        /// </summary>
        /// <param name="reader"></param>
        protected override void LoadContent(PersistenceReader reader)
        {
            reader.OpenScope((PersistItemTag)Persistent.Parameters);
            reader.GetValue(out C);
            reader.GetValue(out cacheSize);
            reader.GetValue(out maximumInput);
            reader.GetValue(out lowerOrder);
            reader.GetValue(out exponent);
            reader.GetValue(out gamma);
            reader.GetValue(out strKernelType);
            reader.CloseScope();

            reader.OpenScope((PersistItemTag)Persistent.Classifiers);
            int length = 0;
            reader.GetValue(out length);
            classifiers = new Avanade.Datamining.SMO.SMO[length][];
            for (int i = 0; i < classifiers.Length; i++)
            {
                classifiers[i] = new Avanade.Datamining.SMO.SMO[length];
                for (int j = i + 1; j < classifiers.Length; j++)
                {
                    classifiers[i][j] = new Avanade.Datamining.SMO.SMO();
                    reader.GetValue(out classifiers[i][j].b);

                    int instLength = 0;
                    reader.GetValue(out instLength);
                    Instances inst = new Instances(new Instance[instLength], this.getLabels(), 0);

                    for (int x = 0; x < instLength; x++)
                    {
                        int posLength = 0;
                        reader.GetValue(out posLength);
                        int[] positions = new int[posLength];
                        double[] values = new double[posLength];
                        for (int y = 0; y < posLength; y++)
                        {
                            reader.GetValue(out positions[y]);
                            reader.GetValue(out values[y]);
                        }
                        int label = 0;
                        reader.GetValue(out label);
                        Instance instance = new Instance(1, label, new double[] { });
                        instance.values = values;
                        instance.positions = positions;
                        inst.instances[x] = instance;
                    }
                    classifiers[i][j].kernel = getKernel(inst);

                    int supportvectorslength = 0;
                    reader.GetValue(out supportvectorslength);
                    LinkedList<int> supportVectors = new LinkedList<int>();
                    for (int x = 0; x < supportvectorslength; x++)
                    {
                        int s = 0;
                        reader.GetValue(out s);
                        supportVectors.AddLast(s);
                    }
                    classifiers[i][j].supportVectors = supportVectors;

                    int alphaLength = 0;
                    reader.GetValue(out alphaLength);
                    double[] alphas = new double[alphaLength];

                    for (int x = 0; x < alphas.Length; x++)
                    {
                        reader.GetValue(out alphas[x]);
                    }
                    classifiers[i][j].alpha = alphas;

                    int labelLength = 0;
                    reader.GetValue(out labelLength);
                    double[] labels = new double[labelLength];

                    for (int x = 0; x < labels.Length; x++)
                    {
                        reader.GetValue(out labels[x]);
                    }
                    classifiers[i][j].labels = labels;
                }
            }
            reader.CloseScope();
        }
        
        /// <summary>
        /// Save an already trained model
        /// </summary>
        /// <param name="writer"></param>
        protected override void SaveContent(PersistenceWriter writer)
        {
            writer.OpenScope((PersistItemTag)Persistent.Parameters);
            writer.SetValue(C);
            writer.SetValue(cacheSize);
            writer.SetValue(maximumInput);
            writer.SetValue(lowerOrder);
            writer.SetValue(exponent);
            writer.SetValue(gamma);
            writer.SetValue(strKernelType);
            writer.CloseScope();

            writer.OpenScope((PersistItemTag)Persistent.Classifiers);

            writer.SetValue(classifiers.Length);
            for (int i = 0; i < classifiers.Length; i++)
            {
                for (int j = i + 1; j < classifiers.Length; j++)
                {
                    writer.SetValue(classifiers[i][j].b);
                    writer.SetValue(classifiers[i][j].kernel.data.instances.Length);

                    for (int x = 0; x < classifiers[i][j].kernel.data.instances.Length; x++)
                    {
                        writer.SetValue(classifiers[i][j].kernel.data.instances[x].positions.Length);
                        for (int y = 0; y < classifiers[i][j].kernel.data.instances[x].positions.Length; y++)
                        {
                            writer.SetValue(classifiers[i][j].kernel.data.instances[x].positions[y]);
                            writer.SetValue(classifiers[i][j].kernel.data.instances[x].values[y]);
                        }
                        writer.SetValue(classifiers[i][j].kernel.data.instances[x].label);
                    }
                    writer.SetValue(classifiers[i][j].supportVectors.Count);
                    foreach (int sup in classifiers[i][j].supportVectors)
                    {
                        writer.SetValue(sup);
                    }

                    writer.SetValue(classifiers[i][j].alpha.Length);

                    for (int x = 0; x < classifiers[i][j].alpha.Length; x++)
                    {
                        writer.SetValue(classifiers[i][j].alpha[x]);
                    }
                    writer.SetValue(classifiers[i][j].labels.Length);

                    for (int x = 0; x < classifiers[i][j].labels.Length; x++)
                    {
                        writer.SetValue(classifiers[i][j].labels[x]);
                    }
                }
            }
            writer.CloseScope();
        }
        #endregion

        #region Prediction
        /// <summary>
        /// The prediction method which can be called from SQL Server
        /// </summary>
        /// <param name="inputCase"></param>
        /// <param name="predictionResult"></param>
        protected override void Predict(MiningCase inputCase, PredictionResult predictionResult) 
        {
            //the input values and predictec label
            double[] values;
            uint label = 0;
            int attributeSize = 0;
            
            //convert and scale the input
            getValues(inputCase, out values, out label, out attributeSize);
            Instance instance = new Instance(1, (int)label, values, getLabels());
            
            //obtain the votes
            int[] votes = getVotes(instance);
            label = 0;
            for (uint i = 1; i < votes.Length; i++)
            {
                if (votes[i] > votes[label])
                {
                    label = i;
                }
            }

            //construct the output
            AttributeStatistics attStats = new AttributeStatistics();
            attStats.Attribute = getTargetAttribute();
            StateStatistics stateStat = new StateStatistics();
            StateValue stateVal = new StateValue();
            stateVal.SetIndex(label);

            stateStat.Value = AttributeSet.TokenizeAttributeValue(attStats.Attribute, AttributeSet.UntokenizeAttributeValue(attStats.Attribute, stateVal));
            attStats.StateStatistics.Add(stateStat);
            predictionResult.AddPrediction(attStats);
        }

        /// <summary>
        /// This method predicts the value of a new example
        /// A voting system is used for the voting
        /// </summary>
        /// <param name="inst">A new example</param>
        /// <returns>the votes</returns>
        public int[] getVotes(Instance inst)
        {
            //create the votes array.. for each class a vote
            int[] votes = new int[inst.labels.Length];
            //determine for each combinations of labels
            //which label is the best
            for (int i = 0; i < inst.labels.Length; i++)
            {
                for (int j = i + 1; j < inst.labels.Length; j++)
                {
                    //call the predict function
                    double output = classifiers[i][j].CalcSVM(-1, inst);
                    //if the example lies above the separating hyperplane give a vote
                    if (output > 0)
                    {
                        votes[j] += 1;
                    }//if it lies below... vote
                    else
                    {
                        votes[i] += 1;
                    }
                }
            }//return the votes
            return votes;
        }

#endregion

        #region [Navigator, DrillThrough and Viewer methods]

        /// <summary>
        /// 
        /// </summary>
        /// <param name="forDMDimensionContent"></param>
        /// <returns></returns>
        protected override AlgorithmNavigationBase GetNavigator(bool forDMDimensionContent)
        {
            return new AlgorithmNavigator(this, forDMDimensionContent);
        }

        /// <summary>
        /// returns the attribute names
        /// </summary>
        /// <returns></returns>
        [MiningFunction("Get all attributes")]
        public System.Data.DataTable GetAttributes()
        {
            //construct the table column names
            System.Data.DataTable table = new System.Data.DataTable();
            
            table.Columns.Add("Columns", typeof(string));
            table.Columns.Add("ColumnType", typeof(string));

            if (Context.ExecuteForPrepare)
            {
                return table;
            }


            for(uint i=0;i<AttributeSet.GetAttributeCount();i++)
            {
                if (!isNominal(i))
                {
                    table.Rows.Add(AttributeSet.GetAttributeDisplayName(i, false), "Continuous");
                }
                else
                {
                    if (!isTarget(i))
                    {
                        table.Rows.Add(AttributeSet.GetAttributeDisplayName(i, false), "Discrete");
                    }
                }
            }
            return table;
        }

        /// <summary>
        /// THis methode retuns all the predict values
        /// </summary>
        /// <returns></returns>
        [MiningFunction("Get all classes")]
        public System.Data.DataTable GetClasses()
        {
            //construct the table column names
            System.Data.DataTable table = new System.Data.DataTable();

            table.Columns.Add("Columns", typeof(string));

            if (Context.ExecuteForPrepare)
            {
                return table;
            }

            
            for(uint i=0;i<this.AttributeSet.GetAttributeStateCount(getTargetAttribute()); i++){
                StateValue value = new StateValue();
                value.SetIndex(i);
                table.Rows.Add(this.AttributeSet.UntokenizeAttributeValue(getTargetAttribute(), value));
            }
            
            return table;
        }

        /// <summary>
        /// This method returns the name of the predict attribute
        /// </summary>
        /// <returns></returns>
        [MiningFunction("Get predict attribute")]
        public System.Data.DataTable GetPredictAttribute()
        {
            //construct the table column names
            System.Data.DataTable table = new System.Data.DataTable();

            table.Columns.Add("Columns", typeof(string));

            if (Context.ExecuteForPrepare)
            {
                return table;
            }

            table.Rows.Add(this.AttributeSet.GetAttributeDisplayName(getTargetAttribute(), false));

            return table;
        }

        /// <summary>
        /// this method returns all the min and max values of the attributes
        /// </summary>
        /// <returns></returns>
        [MiningFunction("Get maximum and minimum values")]
        public System.Data.DataTable GetMaxMinValues()
        {
            //construct the table column names
            System.Data.DataTable table = new System.Data.DataTable();
            
            table.Columns.Add("Column", typeof(string));
            table.Columns.Add("Min", typeof(double));
            table.Columns.Add("Max", typeof(double));

            if (Context.ExecuteForPrepare)
            {
                return table;
            }


            for (uint i = 0; i < this.AttributeSet.GetAttributeCount(); i++)
            {
                table.Rows.Add(this.AttributeSet.GetAttributeDisplayName(i, false), this.MarginalStats.GetAttributeStats(i).Min, this.MarginalStats.GetAttributeStats(i).Max);
            }

            return table;
        }

        /// <summary>
        /// Support drill-through functionality for the viewer
        /// </summary>
        /// <param name="nodeFlags"></param>
        /// <param name="caseID"></param>
        /// <param name="inputCase"></param>
        /// <param name="maxNodes"></param>
        /// <returns></returns>
        protected override string[] GetNodeIDsForCase(
                DrillThroughNodeFlags nodeFlags,
                long caseID, MiningCase inputCase,
                ulong maxNodes)
        {
            return new string[] { "" };

        }
        #endregion

        #region [General methods]
        /// <summary>
        /// Retruns the kernel as specified
        /// </summary>
        /// <param name="instances">Instances used for kernel caching</param>
        /// <returns></returns>
        private Kernel getKernel(Instances instances)
        {
            switch (strKernelType.ToLower())
            {
                case "":
                    return new PolyKernel(instances, cacheSize, 1, false);
                case "linear":
                    return new PolyKernel(instances, cacheSize, 1, false);
                case "polynomial":
                    // verify that the parameter 'exponent' contains a value other than zero.
                    return new PolyKernel(instances, cacheSize, exponent, lowerOrder);
                case "rbf":
                    // verify that the parameter 'gamma' contains a value other than zero.
                    return new RBFKernel(instances, cacheSize, gamma);
                default:
                    throw new Exception("Invalid Kernel Type. In the Parameter Settings the field Kernel_Type should be specified as: 'linear', 'polynomial', or 'rbf'");
            }
        }

        /// <summary>
        /// Check whether an attribute is discrete
        /// </summary>
        /// <param name="attribute">the attribute</param>
        /// <returns>Discrete?</returns>
        protected bool isNominal(uint attribute)
        {
            return ((AttributeFlags.Discrete & AttributeSet.GetAttributeFlags(attribute)) == AttributeFlags.Discrete);
        }


        /// <summary>
        /// Check whether an attribute is the predict attribute
        /// </summary>
        /// <param name="attribute">the attribute</param>
        /// <returns>Predict?</returns>
        protected bool isTarget(uint attribute)
        {
            return ((AttributeFlags.Output & AttributeSet.GetAttributeFlags(attribute)) == AttributeFlags.Output);
        }

        /// <summary>
        /// Returns the target attribute
        /// </summary>
        /// <returns>Target attribute</returns>
        protected uint getTargetAttribute()
        {
            for (uint i = 0; i < AttributeSet.GetAttributeCount(); i++)
            {
                if (isTarget(i))
                {
                    return i;
                }
            }
            throw new Exception("No predict attribute specified");
        }

        /***
         * returns the labels represented by their id (int)
         * 
         * **/
        /// <summary>
        /// Return the id's of the labels of the target attribute
        /// </summary>
        /// <returns>id's of the labels</returns>
        public int[] getLabels()
        {
            int[] output = new int[AttributeSet.GetAttributeStateCount(getTargetAttribute())];
            for (int i = 0; i < output.Length; i++)
            {
                output[i] = i;
            }
            return output;
        }

        /// <summary>
        /// returns the names of the attributes
        /// </summary>
        /// <returns></returns>
        public String[] getAttributeNames()
        {

            LinkedList<string> values = new LinkedList<string>();
            //SortedDictionary<uint, double>.Enumerator enumerator = dict.GetEnumerator();
            for (uint i = 0; i < AttributeSet.GetAttributeCount(); i++)
            {
                if (!isNominal(i))
                {
                    values.AddLast(AttributeSet.GetAttributeDisplayName(i, false));
                }
                else
                {
                    if (!isTarget(i))
                    {
                        for (uint j = 0; j < AttributeSet.GetAttributeStateCount(i); j++)
                        {
                            StateValue value = new StateValue();
                            value.SetIndex(j);
                            object valuea = AttributeSet.UntokenizeAttributeValue(i, value);
                            string name;
                            if (valuea == null)
                            {
                                name = "NULL";
                            }
                            else
                            {
                                name = valuea.ToString();
                                if (name.Equals(""))
                                {
                                    name = "NULL";
                                }
                            }
                            values.AddLast(AttributeSet.GetAttributeDisplayName(i, false) + "." + name);
                        }
                    }
                }
            }
            string[] output = new string[values.Count];
            values.CopyTo(output, 0);
            return output;
        }
        #endregion

    }

}
