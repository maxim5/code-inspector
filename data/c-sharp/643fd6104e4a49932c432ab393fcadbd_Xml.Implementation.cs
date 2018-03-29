namespace CityLizard.Xml
{
    using S = System;

    /// <summary>
    /// Defines the context for a set of XML nodes.
    /// </summary>
    public class Implementation
    {
        /// <summary>
        /// The replaceable error handler.
        /// </summary>
        public S.Action<Node, Node.Error> ErrorHandler { get; set; }

        /// <summary>
        /// Initialize.
        /// </summary>
        public Implementation()
        {
            this.ErrorHandler = (n, e) => { /*throw e;*/ };
        }

        public Linked.Comment Comment(string value)
        {
            var result = new Linked.Comment();
            result.Implementation = this;
            result.Value = value;
            return result;
        }
    }
}
