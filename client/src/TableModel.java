
import javax.swing.table.AbstractTableModel;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Sernheim
 */
public class TableModel extends AbstractTableModel {
    private final String[] columnNames = {"Title", "Artist", "Album", "Duration"};
    private Object[][] data;
    
    public TableModel(Object [][] data){
        this.data = data;
    }

    @Override
    public boolean isCellEditable(int row, int col) {
            return false;
    }

    @Override
    public int getColumnCount() {
        return columnNames.length;
    }

    @Override
    public int getRowCount() {
        return data.length;
    }

    @Override
    public Object getValueAt(int row, int col) {
        return data[row][col];
    }
    
    @Override
    public String getColumnName(int columnIndex) {
        return columnNames[columnIndex];
    }
    public void setValueAt(Object value, int row, int col) {
        data[row][col] = value;
        fireTableCellUpdated(row, col);
    }


};  

