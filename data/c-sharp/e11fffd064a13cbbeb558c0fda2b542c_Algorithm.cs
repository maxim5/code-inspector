using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Design;
using System.Globalization;
using System.Security.Cryptography;
using System.Text;
using System.Windows.Forms;

namespace CryptographyConfig {
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


   public class KeyWrapper : INotifyPropertyChanged {
      private byte[] _key;
      private byte[] _iv;

      public byte[] Key {
         get { return _key; }
         set { _key = value; OnPropertyChanged("Key"); }
      }

      public string KeyString {
         get { return ArrayToString(Key); }
         set { Key = StringToArray(value); OnPropertyChanged("KeyString"); }
      }

      public byte[] IV {
         get { return _iv; }
         set { _iv = value; OnPropertyChanged("IV"); }
      }

      public string IVString {
         get { return ArrayToString(IV); }
         set { IV = StringToArray(value); OnPropertyChanged("IVString"); }
      }

      public byte[] StringToArray(string value) {

         byte[] array = new byte[value.Length / 2];

         for ( int i = 0; i < array.Length; i++ ) {
            array[i] = byte.Parse(value.Substring(i * 2, 2), NumberStyles.HexNumber);
         }

         return array;
      }

      public string ArrayToString(byte[] array) {
         StringBuilder builder = new StringBuilder();
         foreach ( byte b in array ) {
            builder.Append(b.ToString("x"));
         }
         return builder.ToString();
      }

      private void OnPropertyChanged(string name) {
         if ( PropertyChanged != null )
            PropertyChanged(this, new PropertyChangedEventArgs(name));
      }


      #region INotifyPropertyChanged Members

      public event PropertyChangedEventHandler PropertyChanged;

      #endregion
   }

}
