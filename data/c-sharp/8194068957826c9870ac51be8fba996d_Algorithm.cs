using System;
using System.Threading;

namespace dnA.Math {
	/// <summary>
	/// Abstract Algorithm class. Subclasses need to only implement <code>InternalCompute</code>.
	/// </summary>
	public abstract class Algorithm : IAlgorithm {
		private volatile bool computed = false;

		///<summary>Informs user that that computation has began.</summary>
		public event EventHandler BeginComputation;

		///<summary>Informs user that that computation has finished.</summary>
		public event EventHandler EndComputation;

		/// <summary>Handles <c>BeginComputation</c> events.</summary>
		/// <param name="e">event arguments</param>
		protected virtual void OnBeginComputation(EventArgs e) {
			if (BeginComputation != null) {
				BeginComputation(this, e);
			}
		}

		/// <summary>Handles <c>EndComputation</c> events.</summary>
		/// <param name="e">event arguments</param>
		protected virtual void OnEndComputation(EventArgs e) {
			if (EndComputation != null) {
				EndComputation(this, e);
			}
		}

		///<summary>Specific to each algorithm </summary>
		protected abstract void InternalCompute();
       
		/// <summary>Computes the algorithm.</summary>
		public void Compute() {
			if (!computed) {
				lock(this) {
					if (!computed) {
						OnBeginComputation(EventArgs.Empty);
						InternalCompute();
						computed = true;
						OnEndComputation(EventArgs.Empty);
					}
				}
			}
		}
	}
}
