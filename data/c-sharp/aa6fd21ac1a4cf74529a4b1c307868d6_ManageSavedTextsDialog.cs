//	"Manage Saved Texts" dialog
//	--------------------------------------------------------------------------------
//	Porrima Hex Editor
//	Copyright (C) 2008-2010 Stanislav Vorobyev <mailto:stanislav.vorobyev@gmail.com>
//
//	This program is free software: you can redistribute it and/or modify
//	it under the terms of the GNU General Public License as published by
//	the Free Software Foundation, either version 3 of the License, or
//	(at your option) any later version.
//
//	This program is distributed in the hope that it will be useful,
//	but WITHOUT ANY WARRANTY; without even the implied warranty of
//	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//	GNU General Public License for more details.
//
//	You should have received a copy of the GNU General Public License
//	along with this program.  If not, see <http://www.gnu.org/licenses/>.
//	--------------------------------------------------------------------------------
//	$Id: ManageSavedTextsDialog.cs 16 2010-09-12 19:21:39Z stanislav.vorobyev $

using System;
using System.Windows.Forms;
using Porrima.Core;

namespace Porrima.Interface
{
	/// <summary>
	/// ?????? ?????????? ???????????? ????????
	/// </summary>
	internal partial class ManageSavedTextsDialog : Form
	{
		/// <summary>
		/// ????????? ?????, ? ??????? ?? ????????
		/// </summary>
		System.Collections.Specialized.StringCollection collection;

		/// <summary>
		/// ???????????
		/// </summary>
		public ManageSavedTextsDialog()
		{
			InitializeComponent();
		}

		/// <summary>
		/// ????????? ?????? ? ListBox
		/// </summary>
		private void LoadItems()
		{
			collection = Properties.Settings.Default.TextItems;
			foreach (var item in Properties.Settings.Default.TextItems)
			{
				var strippedItem = item.StripText(MainForm.StripTextLength);
				if (strippedItem != item)
					strippedItem += MainForm.ThreeDots;
				mainListBox.Items.Add(strippedItem);
			}
		}

		#region Event handlers

		private void OkButtonClick(object sender, EventArgs e)
		{
			Properties.Settings.Default.TextItems = collection;
			Properties.Settings.Default.Save();
			DialogResult = DialogResult.OK;
		}

		private void DeleteButtonClick(object sender, EventArgs e)
		{
			if (mainListBox.SelectedIndex == -1) return;
			collection.RemoveAt(mainListBox.SelectedIndex);
			mainListBox.Items.RemoveAt(mainListBox.SelectedIndex);
		}

		private void ClearButtonClick(object sender, EventArgs e)
		{
			mainListBox.Items.Clear();
			collection.Clear();
		}

		private void ManageSavedTextsDialogLoad(object sender, EventArgs e)
		{
			LoadItems();
		}

		#endregion
	}
}
