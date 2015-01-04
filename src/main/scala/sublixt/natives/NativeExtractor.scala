package sublixt.natives

import java.io.File
import java.util.zip.ZipFile
import scala.collection.mutable.ArrayBuffer
import java.io.FileOutputStream

object NativeExtractor {
	val tempDir = System.getProperty("java.io.tmpdir")
	val tempFile = new File(tempDir)

	val os = {
		val temp = System.getProperty("os.name").toLowerCase
		if (temp.contains("win")) Some("windows")
		else if (temp.contains("mac")) Some("mac")
		else if (temp.contains("nux")) Some("linux")
		else None
	}

	val arch = {
		val temp = System.getProperty("os.arch")
		if (temp.contains("86")) Some("x86")
		else if (temp.contains("64")) Some("x64")
		else None
	}

	val internalPath =
		for {
			o <- os
			a <- arch
		} yield o + File.separator + a + File.separator

	def extractToTempFromZip(file: File) = {
		if (file.exists()) {
			for (ip <- internalPath) yield {
				try {
					val zip = new ZipFile(file)
					val entries = zip.entries()
					val buffer = new ArrayBuffer[String]()
					while (entries.hasMoreElements()) {
						val entry = entries.nextElement()
						val name = entry.getName.replace("\\", File.separator).replace("/", File.separator)
						if (name.startsWith(ip)) {
							val n = name.drop(ip.length)
							if (n != "") {
								val file = new File(tempFile, n)
								if (!file.exists()) {
									println(n)
									val is = zip.getInputStream(entry)
									val fos = new FileOutputStream(file)
									val buf = new Array[Byte](1024)
									var c = is.read(buf)
									while (c != -1) {
										fos.write(buf, 0, c)
										c = is.read(buf)
									}
									buffer += file.getPath()
								}
							}
						}
					}
					Some(buffer.toList)
				} catch {
					case t: Throwable => None
				}
			}
		} else None
	}
}