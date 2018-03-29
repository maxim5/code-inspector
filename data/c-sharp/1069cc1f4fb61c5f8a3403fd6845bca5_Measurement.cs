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

namespace ProbeNet.Messages.Base
{
    /// <inheritdoc/>
#if !MICRO_FRAMEWORK
    [JsonObject(MemberSerialization.OptIn)]
    [YAXSerializeAs("measurement")]
    [YAXSerializableType(Options = YAXSerializationOptions.DontSerializeNullObjects, FieldsToSerialize = YAXSerializationFields.AttributedFieldsOnly)]
#endif
    public abstract class Measurement : IMeasurement
    {
        /// <summary>
        /// Delegate method for Active Changed Event.
        /// </summary>
        public delegate void ActiveStateChangedDelegate(IMeasurement sender);
        /// <summary>
        /// Occurs when active state changed.
        /// </summary>
        public event ActiveStateChangedDelegate ActiveStateChanged;

        private bool active;

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.Base.Measurement"/> class.
        /// </summary>
        protected Measurement() :
            this(Guid.NewGuid())
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.Base.Measurement"/> class.
        /// </summary>
        /// <param name="uuid">UUID.</param>
        protected Measurement(Guid uuid)
        {
            Uuid = uuid;
            Active = true;
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
            get;
            set;
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
                return active;
            }
            set
            {
                if (active != value) {
                    active = value;
                    NotifyActiveStateChanged();
                }
            }
        }

        /// <summary>
        /// Gets the base alignment.
        /// </summary>
        /// <value>The base alignment.</value>
        protected abstract Alignment BaseAlignment
        {
            get;
        }

        IAlignment IMeasurement.Alignment
        {
            get
            {
                return BaseAlignment;
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
            get;
            set;
        }

        /// <inheritdoc/>
#if MICRO_FRAMEWORK
        public IList Curve
#else
        [JsonProperty("curve", NullValueHandling = NullValueHandling.Ignore)]
        public IList<double[]> Curve
#endif
        {
            get;
            set;
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
                if (Curve == null) {
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
            get;
            set;
        }

        /// <inheritdoc/>
#if !MICRO_FRAMEWORK
        [JsonProperty("tag")]
        [YAXSerializableField]
        [YAXAttributeForClass]
        [YAXSerializeAs("tag")]
#endif
        public string Tag
        {
            get;
            set;
        }
        #endregion

        /// <summary>
        /// Notifies that the active state has changed.
        /// </summary>
        public void NotifyActiveStateChanged()
        {
            if (ActiveStateChanged != null) {
                ActiveStateChanged(this);
            }
        }
    }
}