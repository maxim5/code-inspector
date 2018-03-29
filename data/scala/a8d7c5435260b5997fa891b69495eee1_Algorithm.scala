package org.scajadoc.distributed

/**
 * <p>Interface for all distributed algorithms. Each algorithm implementing
 * algorithm is executed in a separate thread.</p>
 * <p><h1>Algorithm</h1> is xyz.</p>
 *
 *
 * @author Filip Rogaczewski
 * @deprecated don't use that
 * @since 1.0
 * @see org.scajadoc.distributed.DisAlgorithm
 */
trait Algorithm[A] {
	def execute(a : A)
}

/**
 * Distributed algorithm. Executed in a separete thread.
 *
 * @author Filip Rogaczewski
 * @author Filip
 */
trait DisAlgorithm extends Algorithm[ScalaObject]