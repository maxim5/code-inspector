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
using ProbeNet.Messages.Interface;
using ProbeNet.Messages.Wrapper;
#if MICRO_FRAMEWORK
using System.Collections;
#else
using System.Collections.Generic;
using Newtonsoft.Json;
using YAXLib;
#endif

namespace ProbeNet.Messages.SerializationWrapper
{
    /// <summary>
    /// Wraps a Measurement.
    /// </summary>
#if MICRO_FRAMEWORK
    public class Measurement : Wrapper, IMeasurement
#else
    [JsonObject(MemberSerialization.OptIn)]
    [YAXSerializableType(Options = YAXSerializationOptions.DontSerializeNullObjects, FieldsToSerialize = YAXSerializationFields.AttributedFieldsOnly)]
    public class Measurement : Wrapper<IMeasurement>, IMeasurement
#endif
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.SerializationWrapper.Measurement"/> class.
        /// </summary>
        /// <param name="wrapped">Wrapped.</param>
        public Measurement(IMeasurement wrapped) :
            base(wrapped)
        {
        }

        #region IMeasurement implementation
        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty("uuid")]
        [YAXSerializableField]
        [YAXSerializeAs("uuid")]
        [YAXAttributeForClass]
#endif
        public Guid Uuid
        {
            get
            {
#if MICRO_FRAMEWORK
                return ((IMeasurement)Wrapped).Uuid;
#else
                return Wrapped.Uuid;
#endif
            }
        }

        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty("active")]
        [YAXSerializableField]
        [YAXSerializeAs("active")]
        [YAXAttributeForClass]
#endif
        public bool Active
        {
            get
            {
#if MICRO_FRAMEWORK
                return ((IMeasurement)Wrapped).Active;
#else
                return Wrapped.Active;
#endif
            }
        }

        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty("alignment", NullValueHandling = NullValueHandling.Ignore)]
        [YAXSerializableField]
        [YAXSerializeAs("alignment")]
#endif
        public IAlignment Alignment
        {
            get
            {
#if MICRO_FRAMEWORK
                IMeasurement wrapped = (IMeasurement)Wrapped;
#else
                IMeasurement wrapped = Wrapped;
#endif
                if (wrapped.Alignment == null) {
                    return null;
                }
                return new Alignment(wrapped.Alignment);
            }
        }

        /// <inheritdoc/>
#if MICRO_FRAMEWORK
        public IDictionary Results
#else
        [JsonProperty("results")]
        [YAXSerializableField]
        [YAXSerializeAs("results")]
        [YAXDictionary(
            EachPairName = "result",
            KeyName = "id",
            SerializeKeyAs = YAXNodeTypes.Attribute,
            ValueName = "value",
            SerializeValueAs = YAXNodeTypes.Element)]
        public IDictionary<string, Nullable<double>> Results
#endif
        {
            get
            {
#if MICRO_FRAMEWORK
                return ((IMeasurement)Wrapped).Results;
#else
                return Wrapped.Results;
#endif
            }
        }

        /// <inheritdoc/>
#if MICRO_FRAMEWORK
        public IList Curve
#else
        [JsonProperty("curve", NullValueHandling = NullValueHandling.Ignore)]
        public IList<double[]> Curve
#endif
        {
            get
            {
#if MICRO_FRAMEWORK
                return ((IMeasurement)Wrapped).Curve;
#else
                return Wrapped.Curve;
#endif
            }
        }

        /// <inheritdoc/>
#if MICRO_FRAMEWORK
        public IList CurvePoints
#else
        [YAXSerializableField]
        [YAXSerializeAs("curve")]
        [YAXCollection(YAXCollectionSerializationTypes.Recursive, EachElementName = "point")]
        public IList<PointWrapper> CurvePoints
#endif
        {
            get
            {
#if MICRO_FRAMEWORK
                IMeasurement wrapped = (IMeasurement)Wrapped;
#else
                IMeasurement wrapped = Wrapped;
#endif
                if (wrapped.Curve == null) {
                    return null;
                }
                return new CurveWrapper(Curve);
            }
        }

        /// <inheritdoc/>
#if MICRO_FRAMEWORK
        public IDictionary Images
#else
        [JsonProperty("images", NullValueHandling = NullValueHandling.Ignore)]
        [YAXSerializableField]
        [YAXSerializeAs("images")]
        [YAXDictionary(
            EachPairName = "image",
            KeyName = "id",
            SerializeKeyAs = YAXNodeTypes.Attribute,
            ValueName = "data",
            SerializeValueAs = YAXNodeTypes.Attribute)]
        public IDictionary<string, byte[]> Images
#endif
        {
            get
            {
#if MICRO_FRAMEWORK
                return ((IMeasurement)Wrapped).Images;
#else
                return Wrapped.Images;
#endif
            }
        }

        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty("tag")]
#endif
        public string Tag
        {
            get
            {
#if MICRO_FRAMEWORK
                return ((IMeasurement)Wrapped).Tag;
#else
                return Wrapped.Tag;
#endif
            }
        }
        #endregion
    }
}

