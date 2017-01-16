//
// XamlLoader.cs
//
// Author:
//       Stephane Delcroix <stephane@mi8.be>
//
// Copyright (c) 2013 Mobile Inception
// Copyright (c) 2013-2014 Xamarin, Inc
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Xml;

namespace Xamarin.Forms.Xaml.Internals
{
	public static class XamlLoader
	{
		public static Func<Type, string> XamlFileProvider { get; internal set; }
		internal static bool DoNotThrowOnExceptions { get; set; }
	}
}

namespace Xamarin.Forms.Xaml
{
	static class XamlLoader
	{
		static readonly Dictionary<Type, string> XamlResources = new Dictionary<Type, string>();

		public static void Load(object view, Type callingType)
		{
			var xaml = GetXamlForType(callingType);
			if (string.IsNullOrEmpty(xaml))
				throw new XamlParseException(string.Format("No embeddedresources found for {0}", callingType), new XmlLineInfo());
			Load(view, xaml);
		}

		public static void Load(object view, string xaml)
		{
			using (var reader = XmlReader.Create(new StringReader(xaml)))
			{
				while (reader.Read())
				{
					//Skip until element
					if (reader.NodeType == XmlNodeType.Whitespace)
						continue;
					if (reader.NodeType != XmlNodeType.Element)
					{
						Debug.WriteLine("Unhandled node {0} {1} {2}", reader.NodeType, reader.Name, reader.Value);
						continue;
					}

					var rootnode = new RuntimeRootNode (new XmlType (reader.NamespaceURI, reader.Name, null), view, (IXmlNamespaceResolver)reader);
					XamlParser.ParseXaml (rootnode, reader);
					Visit (rootnode, new HydratationContext {
						RootElement = view,
						DoNotThrowOnExceptions = Xamarin.Forms.Xaml.Internals.XamlLoader.DoNotThrowOnExceptions
					});
					break;
				}
			}
		}

		[Obsolete ("Use the XamlFileProvider to provide xaml files. We will remove this when Cycle 8 hits Stable.")]
		public static object Create (string xaml, bool doNotThrow = false)
		{
			object inflatedView = null;
			using (var reader = XmlReader.Create (new StringReader (xaml))) {
				while (reader.Read ()) {
					//Skip until element
					if (reader.NodeType == XmlNodeType.Whitespace)
						continue;
					if (reader.NodeType != XmlNodeType.Element) {
						Debug.WriteLine ("Unhandled node {0} {1} {2}", reader.NodeType, reader.Name, reader.Value);
						continue;
					}

					var rootnode = new RuntimeRootNode (new XmlType (reader.NamespaceURI, reader.Name, null), null, (IXmlNamespaceResolver)reader);
					XamlParser.ParseXaml (rootnode, reader);
					var visitorContext = new HydratationContext {
						DoNotThrowOnExceptions = doNotThrow,
					};
					var cvv = new CreateValuesVisitor (visitorContext);
					cvv.Visit ((ElementNode)rootnode, null);
					inflatedView = rootnode.Root = visitorContext.Values [rootnode];
					visitorContext.RootElement = inflatedView as BindableObject;

					Visit (rootnode, visitorContext);
					break;
				}
			}
			return inflatedView;
		}

		static void Visit (RootNode rootnode, HydratationContext visitorContext)
		{
			rootnode.Accept (new XamlNodeVisitor ((node, parent) => node.Parent = parent), null); //set parents for {StaticResource}
			rootnode.Accept (new ExpandMarkupsVisitor (visitorContext), null);
			rootnode.Accept (new PruneIgnoredNodesVisitor(), null);
			rootnode.Accept (new NamescopingVisitor (visitorContext), null); //set namescopes for {x:Reference}
			rootnode.Accept (new CreateValuesVisitor (visitorContext), null);
			rootnode.Accept (new RegisterXNamesVisitor (visitorContext), null);
			rootnode.Accept (new FillResourceDictionariesVisitor (visitorContext), null);
			rootnode.Accept (new ApplyPropertiesVisitor (visitorContext, true), null);
		}

		static string GetXamlForType(Type type)
		{
			string xaml = null;

			//the Previewer might want to provide it's own xaml for this... let them do that
			if (Xamarin.Forms.Xaml.Internals.XamlLoader.XamlFileProvider != null && (xaml = Xamarin.Forms.Xaml.Internals.XamlLoader.XamlFileProvider(type)) != null)
				return xaml;

			var assembly = type.GetTypeInfo().Assembly;

			string resourceId;
			if (XamlResources.TryGetValue(type, out resourceId))
			{
				var result = ReadResourceAsXaml(type, assembly, resourceId);
				if (result != null)
					return result;
			}

			var likelyResourceName = type.Name + ".xaml";
			var resourceNames = assembly.GetManifestResourceNames();
			string resourceName = null;

			// first pass, pray to find it because the user named it correctly

			foreach (var resource in resourceNames)
			{
				if (ResourceMatchesFilename(assembly, resource, likelyResourceName))
				{
					resourceName = resource;
					xaml = ReadResourceAsXaml(type, assembly, resource);
					if (xaml != null)
						goto end;
				}
			}

			// okay maybe they at least named it .xaml

			foreach (var resource in resourceNames)
			{
				if (!resource.EndsWith(".xaml", StringComparison.OrdinalIgnoreCase))
					continue;

				resourceName = resource;
				xaml = ReadResourceAsXaml(type, assembly, resource);
				if (xaml != null)
					goto end;
			}

			foreach (var resource in resourceNames)
			{
				if (resource.EndsWith(".xaml", StringComparison.OrdinalIgnoreCase))
					continue;

				resourceName = resource;
				xaml = ReadResourceAsXaml(type, assembly, resource, true);
				if (xaml != null)
					goto end;
			}

			end:
			if (xaml == null)
				return null;

			XamlResources[type] = resourceName;
			return xaml;
		}

		static bool ResourceMatchesFilename(Assembly assembly, string resource, string filename)
		{
			try
			{
				var info = assembly.GetManifestResourceInfo(resource);

				if (!string.IsNullOrEmpty(info.FileName) &&
				    string.Compare(info.FileName, filename, StringComparison.OrdinalIgnoreCase) == 0)
					return true;
			}
			catch (PlatformNotSupportedException)
			{
				// Because Win10 + .NET Native
			}

			if (resource.EndsWith("." + filename, StringComparison.OrdinalIgnoreCase) ||
			    string.Compare(resource, filename, StringComparison.OrdinalIgnoreCase) == 0)
				return true;

			return false;
		}

		static string ReadResourceAsXaml(Type type, Assembly assembly, string likelyTargetName, bool validate = false)
		{
			using (var stream = assembly.GetManifestResourceStream(likelyTargetName))
			using (var reader = new StreamReader(stream))
			{
				if (validate)
				{
					// terrible validation of XML. Unfortunately it will probably work most of the time since comments
					// also start with a <. We can't bring in any real deps.

					var firstNonWhitespace = (char)reader.Read();
					while (char.IsWhiteSpace(firstNonWhitespace))
						firstNonWhitespace = (char)reader.Read();

					if (firstNonWhitespace != '<')
						return null;

					stream.Seek(0, SeekOrigin.Begin);
				}

				var xaml = reader.ReadToEnd();

				var pattern = String.Format("x:Class *= *\"{0}\"", type.FullName);
				var regex = new Regex(pattern, RegexOptions.ECMAScript);
				if (regex.IsMatch(xaml) || xaml.Contains(String.Format("x:Class=\"{0}\"", type.FullName)))
					return xaml;
			}
			return null;
		}

		public class RuntimeRootNode : RootNode
		{
			public RuntimeRootNode(XmlType xmlType, object root, IXmlNamespaceResolver resolver) : base (xmlType, resolver)
			{
				Root = root;
			}

			public object Root { get; internal set; }
		}
	}
}