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
#if !MICRO_FRAMEWORK
using System.ComponentModel;
#endif

namespace ProbeNet.Messages
{
    /// <summary>
    /// Error type.
    /// </summary>
    public enum ErrorType
    {
        /// <summary>
        /// The identifier not found.
        /// </summary>
#if !MICRO_FRAMEWORK
        [Description("identifier-not-found")]
#endif
        IdentifierNotFound,

        /// <summary>
        /// The type of the incompatible message.
        /// </summary>
#if !MICRO_FRAMEWORK
        [Description("incompatible-message-type")]
#endif
        IncompatibleMessageType
    }
}

