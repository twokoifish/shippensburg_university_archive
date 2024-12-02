package datasource;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import datadto.CompoundMadeOfDTO;

/**
 * Table Data Gateway for CompoundMadeOf Table.
 * @author Joel
 *
 */
public class CompoundMadeOfTableDataGatewayRDS{
	
	
	
	/**
	 * Creates the table in the database. Drops the table if it already exists.
	 * @throws DatabaseException
	 */
	public static void createTable() throws DatabaseException {
		String drop = "DROP TABLE IF EXISTS CompoundMadeOf";
		String create = "CREATE TABLE CompoundMadeOf (" + 
				"compoundID INT NOT NULL," + 
				"elementID INT," + 
				"metalID INT," + 
				"FOREIGN KEY(compoundID) REFERENCES Compound(compoundID), " +
				"FOREIGN KEY(elementID) REFERENCES Element(elementID), "  +
				"FOREIGN KEY(metalID) REFERENCES Metal(metalID));" ;
				
		try
		{
			// drop table
			PreparedStatement stmt;
			stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(drop);
			stmt.execute();
			stmt.close();

			// create table
			stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(create);
			stmt.execute();
			stmt.close();
		} catch (SQLException e) {
			throw new DatabaseException("Unable to create the CompoundMadeOf table", e);
		}
	}
	
	/**
	 * Only drops the table.
	 * @throws DatabaseException
	 */
	public static void dropTable() throws DatabaseException {
	  try
    {
  	  String drop = "DROP TABLE IF EXISTS CompoundMadeOf";
  	  PreparedStatement stmt;
      stmt = DatabaseManager.getSingleton().getConnection().prepareStatement(drop);
      stmt.execute();
      stmt.close();
    }catch (SQLException e) {
      throw new DatabaseException("Unable to drop the CompoundMadeOf table", e);
    }
	}

//	List<CompoundMadeOfDTO> dtoList = new ArrayList<CompoundMadeOfDTO>();
//	private int compoundID;
//	
//	/**
//	 * Constructs CompoundMadeOf Gateway view based off of matches with given ID.
//	 * @param id
//	 * @throws DatabaseException
//	 */
//	public CompoundMadeOfTableDataGatewayRDS(int id) throws DatabaseException {
//		compoundID = id;
//		findCompounds();
//	}
	
	/**
	 * Finds compounds and adds to dtoList.
	 * @throws DatabaseException
	 */
	public static List<CompoundMadeOfDTO> findCompoundsByCompound(int compoundID){
		
		try {
		  Connection conn = DatabaseManager.getSingleton().getConnection();
			PreparedStatement stmt = conn.prepareStatement("SELECT * FROM CompoundMadeOf WHERE compoundID = " + compoundID);
			
			ResultSet rs = stmt.executeQuery();
			List<CompoundMadeOfDTO> dtoList = new ArrayList<CompoundMadeOfDTO>();
			while(rs.next()) {
			  int comID = rs.getInt("compoundID");
        int elmID = rs.getInt("elementID");
        if(rs.wasNull()) {
          elmID = -1;
        }
        int metID = rs.getInt("metalID");
        if(rs.wasNull()) {
          metID = -1;
        }   
        dtoList.add(new CompoundMadeOfDTO(comID, elmID, metID));
			}
			return dtoList;
		} catch (SQLException | DatabaseException e) {
			new DatabaseException("could not get CompoundMadeOf");
		}
		return null;
	}
	
	public static List<CompoundMadeOfDTO> findCompoundsByElement(int elementID) {
	 
    try {
      Connection conn = DatabaseManager.getSingleton().getConnection();
      PreparedStatement stmt = conn.prepareStatement("SELECT * FROM CompoundMadeOf WHERE elementID = " + elementID);
      
      ResultSet rs = stmt.executeQuery();
      List<CompoundMadeOfDTO> dtoList = new ArrayList<CompoundMadeOfDTO>();
      while(rs.next()) {
        int comID = rs.getInt("compoundID");
        int elmID = rs.getInt("elementID");

        dtoList.add(new CompoundMadeOfDTO(comID, elmID, -1));
      }
      return dtoList;
    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get CompoundMadeOf");
    }
    return null;
  }
	
	public static List<CompoundMadeOfDTO> findCompoundsByMetal(int metalID) {
	   
    try {
      Connection conn = DatabaseManager.getSingleton().getConnection();
      PreparedStatement stmt = conn.prepareStatement("SELECT * FROM CompoundMadeOf WHERE metalID = " + metalID);
      
      ResultSet rs = stmt.executeQuery();
      List<CompoundMadeOfDTO> dtoList = new ArrayList<CompoundMadeOfDTO>();
      while(rs.next()) {
        int comID = rs.getInt("compoundID");
        int metID = rs.getInt("metalID");

        dtoList.add(new CompoundMadeOfDTO(comID, -1, metID));
      }
      return dtoList;
    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get CompoundMadeOf");
    }
    return null;
  }
	
	public static synchronized void addCompoundMadeOf(int compoundID, int elementOrMetalID, String s) {
		try {
		  Connection conn = DatabaseManager.getSingleton().getConnection();
		  String elementIDString = "null";
		  String metalIDString = "null";
		  if(s.contains("Element")) {
		    elementIDString = elementOrMetalID + "";
		  }
		  else if(s.contains("Metal")) {
		    metalIDString = elementOrMetalID + "";
		  }
		  //doesn't add if allready present
		  PreparedStatement stmt = conn.prepareStatement("SELECT * FROM CompoundMadeOf WHERE elementID = " + elementIDString + " AND compoundID = " + compoundID + " AND metalID = " + metalIDString);
		  ResultSet rs = stmt.executeQuery();
		  if(!rs.next()) {
		    stmt = conn.prepareStatement("INSERT INTO CompoundMadeOf Values (" + compoundID + ", " + elementIDString + ", " + metalIDString +");");
	      stmt.execute();
		  }
			
		}catch(SQLException | DatabaseException e) {
			new DatabaseException("couldn't insert into CompoundMadeOf");
		}
	}
	
	public static void deleteCompound(int compoundID) {
	  try {
      Connection conn = DatabaseManager.getSingleton().getConnection();
      PreparedStatement stmt = conn.prepareStatement("DELETE FROM CompoundMadeOf WHERE compoundID = " + compoundID);
      
      stmt.execute();
    } catch (SQLException | DatabaseException e) {
      new DatabaseException("could not get CompoundMadeOf");
    }
    
	}
}
