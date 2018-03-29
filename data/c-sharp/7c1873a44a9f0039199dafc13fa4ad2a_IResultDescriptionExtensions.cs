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
#if MICRO_FRAMEWORK
using System.Collections;
#else
using System.Collections.Generic;
#endif

namespace ProbeNet.Messages
{
    /// <summary>
    /// Extensions for the <see cref="IResultDescription"/> interface.
    /// </summary>
    public static class IResultDescriptionExtensions
    {
        /// <summary>
        /// Finds the result description by identifier.
        /// </summary>
        /// <returns>The result description.</returns>
        /// <param name="resultDescriptions">Result descriptions.</param>
        /// <param name="id">Identifier.</param>

#if MICRO_FRAMEWORK
        public static IResultDescription FindResultDescriptionById(this IList resultDescriptions,
#else
        public static IResultDescription FindById(this IList<IResultDescription> resultDescriptions,
#endif
 string id)
        {
#if MICRO_FRAMEWORK
            foreach (object rd in resultDescriptions) {
                IResultDescription resultDescription = rd as IResultDescription;
#else
            foreach (IResultDescription resultDescription in resultDescriptions) {
#endif
                if (resultDescription != null && resultDescription.Id == id) {
                    return resultDescription;
                }
            }
            return null;
        }

        /// <summary>
        /// Gets the index of the result description with the specified id.
        /// </summary>
        /// <returns>The index.</returns>
        /// <param name="resultDescriptions">Result descriptions.</param>
        /// <param name="id">Identifier.</param>
        public static int GetIndexOf(
#if MICRO_FRAMEWORK
this IList resultDescriptions,
#else
this IList<IResultDescription> resultDescriptions,
#endif
 string id)
        {
#if MICRO_FRAMEWORK
            IResultDescription resultDescription = resultDescriptions.FindResultDescriptionById(id);
#else
            IResultDescription resultDescription = resultDescriptions.FindById(id);
#endif
            if (resultDescription == null) {
#if MICRO_FRAMEWORK
                string message = "No such id found: " + id;
#else
                string message = String.Format("No such id found: \"{0}\"", id);
#endif
                throw new ArgumentException(message);
            }
            return resultDescriptions.IndexOf(resultDescription);
        }
    }
}

