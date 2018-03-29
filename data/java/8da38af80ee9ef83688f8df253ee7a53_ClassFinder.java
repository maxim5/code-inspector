/*
 *  stream.ai
 *
 *  Copyright (C) 2011-2012 by Christian Bockermann, Hendrik Blom
 * 
 *  stream.ai is a library, API and runtime environment for processing high
 *  volume data streams. It is composed of three submodules "stream-api",
 *  "stream-core" and "stream-runtime".
 *
 *  The stream.ai library (and its submodules) is free software: you can 
 *  redistribute it and/or modify it under the terms of the 
 *  GNU Affero General Public License as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any 
 *  later version.
 *
 *  The stream.ai library (and its submodules) is distributed in the hope
 *  that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program.  If not, see http://www.gnu.org/licenses/.
 */
package com.rapidminer.beans.utils;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author chris
 * 
 */
public class ClassFinder {

	static Logger log = LoggerFactory.getLogger(ClassFinder.class);

	static class CustomClassLoader extends ClassLoader {
		public CustomClassLoader(ClassLoader parent) {
			super(parent);
		}

		public String[] getPackageNames() {
			Package[] pkgs = getPackages();
			String[] names = new String[pkgs.length + 1];
			names[0] = "";
			for (int i = 0; i < pkgs.length; i++) {
				names[i + 1] = pkgs[i].getName();
			}
			return names;
		}
	}

	public static Set<Class<?>> getClasses(ClassLoader classLoader) {
		Set<Class<?>> found = new LinkedHashSet<Class<?>>();

		CustomClassLoader ccl = new CustomClassLoader(classLoader);
		log.debug("Packages: {}", (Object[]) ccl.getPackageNames());
		found.addAll(getClasses(ccl.getPackageNames(), ccl));
		return found;
	}

	public static Set<Class<?>> getClasses(String[] packages,
			ClassLoader classLoader) {
		Set<Class<?>> found = new LinkedHashSet<Class<?>>();
		for (String pkg : packages) {
			try {
				Class<?>[] classes = getClasses(pkg, classLoader);
				for (Class<?> clazz : classes) {
					if (!found.contains(clazz))
						found.add(clazz);
				}
			} catch (Exception e) {
				log.error("Error while searching for classes: {}",
						e.getMessage());
				if (log.isDebugEnabled())
					e.printStackTrace();
			}
		}

		return found;
	}

	/**
	 * Scans all classes accessible from the context class loader which belong
	 * to the given package and subpackages.
	 * 
	 * @param packageName
	 *            The base package
	 * @return The classes
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	public static Class<?>[] getClasses(String packageName,
			ClassLoader classLoader) throws ClassNotFoundException, IOException {
		ArrayList<Class<?>> classes = new ArrayList<Class<?>>();
		// ClassLoader classLoader = ClassFinder.class.getClassLoader(); //
		// .getContextClassLoader();
		log.debug("Using class-loader {}", classLoader);
		assert classLoader != null;
		List<URL> resources = new ArrayList<URL>();
		String path = packageName.replace('.', '/');
		Enumeration<URL> urlList = classLoader.getResources(path);
		while (urlList.hasMoreElements()) {
			resources.add(urlList.nextElement());
		}

		if (classLoader instanceof URLClassLoader) {
			URLClassLoader ucl = (URLClassLoader) classLoader;
			URL[] urls = ucl.getURLs();
			if (urls != null) {
				for (URL url : urls) {
					log.debug("Adding URL {} from URLClassLoader", url);
					resources.add(url);
				}
			}
		}

		List<File> dirs = new ArrayList<File>();
		for (URL resource : resources) {
			if (resource.toString().startsWith("jar:")
					|| resource.toExternalForm().endsWith(".jar")) {
				log.debug("Scanning jar-file {}", resource.getPath());

				String p = resource.getPath();
				if (p.indexOf("!") > 0) {
					p = p.substring(0, p.indexOf("!"));
					log.trace("Opening jar '{}'", p);
				}
				if (p.startsWith("file:"))
					p = p.substring("file:".length());

				classes.addAll(findClasses(new JarFile(p), packageName,
						classLoader));
			} else {
				log.trace("Checking URL {}", resource);
				dirs.add(new File(resource.getFile()));
			}
		}

		for (File directory : dirs) {
			List<Class<?>> cl = findClasses(directory, packageName, classLoader);
			log.info("Found {} classes in {}", cl.size(), directory);
			classes.addAll(cl);
		}
		return classes.toArray(new Class[classes.size()]);
	}

	public static List<Class<?>> findClasses(JarFile jar, String packageName,
			ClassLoader classLoader) throws ClassNotFoundException {
		List<Class<?>> classes = new ArrayList<Class<?>>();
		log.debug("Checking jar-file {}", jar.getName());
		Enumeration<JarEntry> en = jar.entries();
		while (en.hasMoreElements()) {

			JarEntry entry = en.nextElement();
			entry.getName();
			log.trace("Checking JarEntry '{}'", entry.getName());

			if (entry.getName().endsWith(".class")
					&& entry.getName().indexOf("$") < 0
					&& entry.getName().replaceAll("/", ".")
							.startsWith(packageName)) {
				try {
					String className = entry.getName()
							.replaceAll("\\.class$", "").replaceAll("/", ".");
					log.trace("Class-name is: '{}'", className);

					Class<?> clazz = Class.forName(className, false,
							classLoader);
					log.trace("Found class {}", clazz);
					classes.add(clazz);
				} catch (NoClassDefFoundError ncdfe) {
				} catch (ClassNotFoundException cnfe) {
				} catch (Exception e) {
					log.error("Failed to load class for entry '{}': {}",
							entry.getName(), e.getMessage());
					e.printStackTrace();
				}
			}

		}

		return classes;
	}

	/**
	 * Recursive method used to find all classes in a given directory and
	 * subdirs.
	 * 
	 * @param directory
	 *            The base directory
	 * @param packageName
	 *            The package name for classes found inside the base directory
	 * @return The classes
	 * @throws ClassNotFoundException
	 */
	private static List<Class<?>> findClasses(File directory,
			String packageName, ClassLoader classLoader)
			throws ClassNotFoundException {

		log.debug("Searching directory '{}' for package '{}'", directory,
				packageName);

		List<Class<?>> classes = new ArrayList<Class<?>>();
		if (!directory.exists()) {
			return classes;
		}
		File[] files = directory.listFiles();
		if (files == null)
			return classes;

		for (File file : files) {
			if (file.isDirectory()) {
				assert !file.getName().contains(".");

				if (packageName.isEmpty()) {
					classes.addAll(findClasses(file, file.getName(),
							classLoader));
				} else {
					classes.addAll(findClasses(file,
							packageName + "." + file.getName(), classLoader));
				}
			} else if (file.getName().endsWith(".class")) {
				try {
					String className = packageName
							+ '.'
							+ file.getName().substring(0,
									file.getName().length() - 6);

					log.debug("Checking element {}", className);

					if (className.indexOf("$") >= 0) {
						continue;
					}

					while (className.startsWith("."))
						className = className.substring(1);

					log.trace("Loading class '{}'", className);
					classes.add(Class.forName(className, false, classLoader));
				} catch (NoClassDefFoundError ncdfe) {
				} catch (ClassNotFoundException cnfe) {
				} catch (Exception e) {
					log.error("Failed to add class: {}", e.getMessage());
				}
			}
		}
		return classes;
	}

	public static void main(String[] args) {
		long start = System.currentTimeMillis();
		Set<Class<?>> classes = ClassFinder.getClasses(ClassFinder.class
				.getClassLoader());
		long time = System.currentTimeMillis() - start;
		log.info("Searching class-path took {} ms", time);
		if (log.isDebugEnabled()) {
			for (Class<?> cl : classes) {
				log.debug("{}", cl);
			}
		}
	}
}