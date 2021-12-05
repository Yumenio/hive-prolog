column_names = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

def print_board(board):
    rows = len(board)
    cols = len(board[0])
    indent = 0
    headings = " "*5+(" "*3).join(column_names[:cols])
    print(headings)
    tops = " "*5+(" "*3).join("-"*cols)
    print(tops)
    roof = " "*4+"/ \\"+"_/ \\"*(cols-1)
    print(roof)
    color_mapping = lambda i : " WB"[i]
    for r in range(rows):
        row_mid = " "*indent
        row_mid += " {} | ".format(r+1)
        row_mid += " | ".join(map(color_mapping,board[r]))
        row_mid += " | {} ".format(r+1)
        print(row_mid)
        row_bottom = " "*indent
        row_bottom += " "*3+" \\_/"*cols
        if r<rows-1:
            row_bottom += " \\"
        print(row_bottom)
        indent += 2
    headings = " "*(indent-2)+headings
    print(headings)
    
board=[[0,2,0,0],[0,0,0,1],[0,0,0,2],[1,2,0,0],[0,2,1,0]]
print_board(board)