/*
 * Algorithm.cs
 * 
 * Copyright (c) 2003-2005, dnAnalytics. All rights reserved.
*/
using System;

namespace dnAnalytics.Math
{
    /// <summary>
    /// Abstract Algorithm class. Subclasses need to only implement <see cref="InternalCompute()"/>.
    /// </summary>
    public abstract class Algorithm : IAlgorithm
    {
        private volatile bool computed = false;

        ///<summary>Specific to each algorithm </summary>
        protected abstract void InternalCompute();

        /// <summary>Computes the algorithm.</summary>
        public void Compute()
        {
            lock (this)
            {
                if (!computed)
                {
                    InternalCompute();
                    computed = true;
                }
            }
        }
    }
}