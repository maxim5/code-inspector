using System;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.Text;


namespace Winterdom.BizTalk.CryptoPipeline {
   /// <summary>
   /// Cryptographic Algorithm to use
   /// </summary>
   public enum Algorithm {
      DES,
      TripleDES,
      Rijndael,
      RC2
   } // enum Algorithm

   /// <summary>
   /// Helper factory class to create symmetric algorithms
   /// </summary>
   internal sealed class AlgorithmProvider {
      public static SymmetricAlgorithm Create(Algorithm algorithm) {
         return SymmetricAlgorithm.Create(algorithm.ToString());
      }

   } // class AlgorithmProvider
}
