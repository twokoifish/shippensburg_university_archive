import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.HashMap;

/**
 * Manages all of the files that are open on this server
 * 
 * @author merlin
 *
 */
public class FileManager
{

	private class FileInformation
	{
		int fileID;
		String fileTitle;
		RandomAccessFile file;
		int numberOfTimesOpen;

		public FileInformation(String fileTitle) throws DFSFileDoesntExist
		{
			super();
			synchronized (singleton)
			{
				this.fileID = nextFileID;
				nextFileID++;
				this.fileTitle = fileTitle;
				this.numberOfTimesOpen = 1;
				try
				{
					file = new RandomAccessFile(new File(fileTitle), "r");
				} catch (FileNotFoundException e)
				{
					throw new DFSFileDoesntExist(fileTitle);
				}
			}
		}

		void decrementOpenCount()
		{
			numberOfTimesOpen--;
		}

		void incrementOpenCount()
		{
			numberOfTimesOpen++;
		}
	}

	private static FileManager singleton;

	private static String pathToFiles = "../data/";

	private static int nextFileID = 1;

	/**
	 * @return the only one of these that can exist
	 */
	public static FileManager getSingleton()
	{
		if (singleton == null)
		{
			singleton = new FileManager();
		}
		return singleton;
	}

	/**
	 * Used only for testing - throws away the current state of the singleton
	 */
	public static void resetSingleton()
	{
		singleton = null;
	}

	private HashMap<Integer, FileInformation> mapByFileID;

	private HashMap<String, FileInformation> mapByFileTitle;

	private FileManager()
	{
		mapByFileID = new HashMap<Integer, FileInformation>();
		mapByFileTitle = new HashMap<String, FileInformation>();
	}

	private String fullFileTitle(String fileTitle)
	{
		return pathToFiles + fileTitle;
	}

	/**
	 * @param fileID
	 *            the file we care about
	 * @return the number of clients that currently have it open
	 */
	public int getNumberOfTimesOpen(int fileID)
	{
		FileInformation fileInformation = mapByFileID.get(fileID);
		return fileInformation.numberOfTimesOpen;
	}

	/**
	 * @param fileTitle
	 *            the name of the file
	 * @return the number of clients that currently have it open
	 */
	public int getNumberOfTimesOpen(String fileTitle)
	{
		FileInformation fileInformation = mapByFileTitle.get(fullFileTitle(fileTitle));
		return fileInformation.numberOfTimesOpen;
	}

	private void insert(FileInformation fileInfo)
	{
		synchronized (singleton)
		{
			mapByFileID.put(fileInfo.fileID, fileInfo);
			mapByFileTitle.put(fileInfo.fileTitle, fileInfo);
		}
	}

	/**
	 * open a file for read access.
	 * 
	 * @param fileTitle
	 *            the name of the file
	 * @return a file identifier you can use to access the file
	 * @throws DFSFileDoesntExist
	 *             if we can't find a file with that name
	 */
	public int openForRead(String fileTitle) throws DFSFileDoesntExist
	{
		FileInformation fileInfo;
		fileTitle = fullFileTitle(fileTitle);
		if (mapByFileTitle.containsKey(fileTitle))
		{
			fileInfo = mapByFileTitle.get(fileTitle);
			fileInfo.incrementOpenCount();
		} else
		{
			fileInfo = new FileInformation(fileTitle);
			insert(fileInfo);
		}
		return fileInfo.fileID;
	}

	/**
	 * Read from a file that is open
	 * 
	 * @param fileID
	 *            the file
	 * @param filePointer
	 *            offset into the file we should start at
	 * @param chars
	 *            the number of characters we should read
	 * @return the data we read
	 */
	public String readFrom(int fileID, int filePointer, int chars)
	{
		RandomAccessFile file = mapByFileID.get(fileID).file;
		byte[] data = new byte[chars];
		try
		{
			file.seek(filePointer);
			file.read(data, 0, chars);
		} catch (IOException e)
		{
			e.printStackTrace();
			System.exit(-1);
		}
		return new String(data);
	}

	void setPathToFiles(String path)
	{
		pathToFiles = path;
	}

	public void closeFile(int fileID)
	{
		FileInformation fileInformation = mapByFileID.get(fileID);
		fileInformation.decrementOpenCount();
		if (fileInformation.numberOfTimesOpen == 0)
		{
			mapByFileID.remove(fileID);
			mapByFileTitle.remove(fileInformation.fileTitle);
		}
	}

	public boolean contains(int fileID)
	{
		return mapByFileID.containsKey(fileID);
	}

}
