/*
 * Data types for the ProbeNet Protocol in C#
 * Copyright (C) Wolfgang Silbermayr
 * Copyright (C) Florian Marchl
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
using System;
using System.IO;
using ProbeNet.Messages.Raw;
#if MICRO_FRAMEWORK
using System.Collections;
#else
using System.Collections.Generic;
using Newtonsoft.Json;
#endif

namespace ProbeNet.Messages
{
    /// <summary>
    /// Measurement collection.
    /// </summary>
#if !MICRO_FRAMEWORK
	[JsonObject(MemberSerialization.OptIn)]
#endif
    public class MeasurementCollection
    {
        private MeasurementDescription description;
#if MICRO_FRAMEWORK
        private IList measurements;
#else
		private List<Measurement> measurements;
#endif

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.MeasurementCollection"/> class.
        /// </summary>
        public MeasurementCollection()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.MeasurementCollection"/> class.
        /// </summary>
        /// <param name="description">Measurement description.</param>
        /// <param name="measurements">Measurement list.</param>
        public MeasurementCollection(MeasurementDescription description,
#if MICRO_FRAMEWORK
 IList measurements
#else
            List<Measurement> measurements
#endif
)
        {
            this.description = description;
            this.measurements = measurements;
        }

        /// <summary>
        /// Gets or sets the description.
        /// </summary>
        /// <value>The description.</value>
#if !MICRO_FRAMEWORK
		[JsonProperty("description")]
#endif
        public MeasurementDescription Description
        {
            get
            {
                return this.description;
            }
            set
            {
                description = value;
            }
        }

        /// <summary>
        /// Gets or sets the measurements.
        /// </summary>
        /// <value>The measurements.</value>
#if MICRO_FRAMEWORK
        public IList Measurements
#else
		[JsonProperty("measurements")]
		public List<Measurement> Measurements
#endif
        {
            get
            {
                return this.measurements;
            }
            set
            {
                measurements = value;
            }
        }


#if !MICRO_FRAMEWORK
        /// <summary>
        /// Loads a measuremetn collection from file.
        /// </summary>
        /// <returns>The measurement collection.</returns>
        /// <param name="fileName">Name of the source file.</param>
        public static MeasurementCollection LoadFromFile(string fileName)
        {
            return DeserializeFile<MeasurementCollection>(fileName);
        }

		private static T DeserializeFile<T>(string fileName)
			where T: new()
		{
			FileInfo fileInfo = new FileInfo(fileName);
			if (fileInfo.Length == 0) {
				return new T();
			}

			FileStream readStream = new FileStream(fileName, FileMode.Open);

			try
			{
				return DeserializeStream<T>(readStream);
			}
			catch(Exception e)
			{
				throw e;
			}
			finally
			{
				readStream.Close();
			}
		}

		private static T DeserializeStream<T>(Stream stream)
		{
			TextReader textReader = new StreamReader(stream);

			try
			{
				JsonSerializer serializer = new JsonSerializer();
				object o = serializer.Deserialize(textReader, typeof(T));
				return (T)o;
			}
			catch(Exception e)
			{
				throw e;
			}
			finally
			{
				textReader.Close();
			}
		}

        /// <summary>
        /// Saves measurement collection to a file.
        /// </summary>
        /// <param name="fileName">The file name.</param>
		public void SaveToFile(string fileName)
		{
			SerializeFile(fileName, this);
		}
		
		private void SerializeFile(string fileName, MeasurementCollection collection)
		{
			FileStream writeStream = new FileStream(fileName, FileMode.Create);

			try {
				SerializeStream(writeStream, collection);
			}
			catch(Exception e)
			{
				throw e;
			}
			finally {
				writeStream.Close();
			}
		}

        /// <summary>
        /// Serializes the stream.
        /// </summary>
        /// <param name="writeStream">Write stream.</param>
        /// <param name="collection">Collection.</param>
		public void SerializeStream(Stream writeStream, MeasurementCollection collection)
		{
			TextWriter textWriter = new StreamWriter(writeStream);
			JsonTextWriter jsonWriter = new JsonTextWriter(textWriter);
			jsonWriter.Formatting = Formatting.Indented;

			try {
				JsonSerializer serializer = new JsonSerializer();
				serializer.Serialize(jsonWriter, collection);
			}
			catch(Exception e)
			{
				throw e;
			}
			finally {
				textWriter.Close();
			}
		}
#endif
    }
}

