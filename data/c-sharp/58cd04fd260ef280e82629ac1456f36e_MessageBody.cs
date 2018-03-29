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
using ProbeNet.Enums;
#if MICRO_FRAMEWORK
using System.Collections;
using ProbeNet.Enums.Converting;
#else
using Frank.Helpers.Json;
using Newtonsoft.Json;
#endif

namespace ProbeNet.Messages
{
    /// <summary>
    /// Message body.
    /// </summary>
#if !MICRO_FRAMEWORK
    [JsonObject(MemberSerialization.OptIn)]
#endif
    public class MessageBody : IMessageBody
    {
        private MessageType type;

        /// <summary>
        /// Initializes a new instance of the <see cref="ProbeNet.Messages.MessageBody"/> class.
        /// </summary>
        /// <param name="type">Type.</param>
        public MessageBody(MessageType type)
        {
            this.type = type;
        }

#if MICRO_FRAMEWORK
        /// <summary>
        /// Initialize a new instance and set values according to specified hashtable
        /// </summary>
        /// <param name="deserialized">The deserialized values.</param>
        public MessageBody(Hashtable deserialized)
        {
            Type = (string)deserialized["type"];
        }
#endif

        /// <inheritdoc/>
#if MICRO_FRAMEWORK
        public string Type
        {
            get
            {
                string result = (string)MessageTypeConverter.Serialize(type);
                result = result.Trim('\"');
                return result;
            }
            set
            {
                value = value.Trim('\"');
                type = MessageTypeConverter.Deserialize(value);
            }
        }
#else
        [JsonProperty("type")]
        [JsonConverter(typeof(JsonDescriptionEnumConverter<MessageType>))]
        public MessageType Type
        {
            get
            {
                return type;
            }
            set
            {
                type = value;
            }
        }
#endif
    }
}

