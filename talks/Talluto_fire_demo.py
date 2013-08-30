#!/usr/bin/python

# contact: matthew.talluto@uqar.ca

import random
import math
import Tkinter

def main():
    # model settings
    numberOfYears = 200
    gridSize = 64
    meanFireSize = 9.14901
    fireProbability = 0.54607
    meanNumberOfFires = 2.16358
    neighborDistance = 1
    regenerationProbability = 0.01
    
    # set up the grid
    grid = Grid(gridSize, neighborDistance, fireProbability, meanNumberOfFires, meanFireSize, regenerationProbability)
    
    # main loop in time
    for year in range(numberOfYears):
        grid.update()
        
    raw_input("Model finished; <enter> to exit")            


class Fire(object):
    
    def __init__(self, _grid, _size):
        self.grid = _grid
        self.burned = []
        self.neighbors = []
        self.targetSize = _size
        
    def current_size(self):
        return len(self.burned)
        
    def start(self, startLocation):
        burning = True
        self.currentCell = startLocation
        while burning:
            self.burn()
            if (self.current_size() >= self.targetSize) or (len(self.neighbors) == 0):  # fire burns out
                burning = False
            else:    # fire spreads
                newIndex = random.randrange(len(self.neighbors))  # pick index to spread to
                self.currentCell = self.neighbors[newIndex]       # assign the new cell to currentCell
                del(self.neighbors[newIndex])                     # remove it from the neighbor list
        # reset visited flag
        for c in self.burned:
            c.fireVisited = False
        return self.burned
    
    def burn(self):
        self.currentCell.burning = True
        self.burned.append(self.currentCell)
        # find new neighbors to burn
        for potentialNeighbor in self.currentCell.neighbors:
            if (not potentialNeighbor.fireVisited) and (potentialNeighbor.can_burn()):
                potentialNeighbor.fireVisited = True
                self.neighbors.append(potentialNeighbor)
        self.grid.display.update_cell(self.currentCell)
        self.grid.display.displayWindow.tkupdate()
       
        
class Grid(object):
    def __init__(self, size, neighborDistance, fireProb, numFires, fireSize, regenProb):
        print "Creating grid..."
        self.cells = []
        self.emptyCells = []
        self.display = GridDisplay(self, (size, size), 8)
        self.fireProbability = fireProb
        self.meanNumberOfFires = numFires
        self.meanFireSize = fireSize
        self.regenerationProbability = regenProb
        # make new cells
        print "Creating cells..."
        for x in range(size):
            self.cells.append([])
            for y in range(size):
                self.cells[x].append(Cell(x, y))
        self.display.update()
        # find all of the neighbors for each cell
        print "Finding neighbors..."
        self.find_neighbors(neighborDistance)

    def find_neighbors(self, dist):
        offsets = calculate_offsets(dist)
        xlim = (0, len(self.cells))
        for col in self.cells:
            ylim = (0, len(col))
            for cell in col:
                for o in offsets:
                    newx = cell.x + o[0]
                    newy = cell.y + o[1]
                    if newx >= xlim[0] and newx < xlim[1] and newy >= ylim[0] and newy < ylim[1]:
                        cell.neighbors.append(self.cells[newx][newy])
    
    def update(self):
        burningCells = []
        
        # select how many fires occurred by drawing from the zero inflated poisson distribution
        numFires = rzip(self.fireProbability, self.meanNumberOfFires)
        
        for i in range(numFires):
            # choose a fire size from negative exponential distribution, rounded up to the nearest whole cell
            fireSize = math.ceil(random.expovariate(1.0 / self.meanFireSize))

            # select a starting cell
            startingCell = random.choice(random.choice(self.cells))
            while startingCell.burning and (len(burningCells) < len(self.cells)):
                startingCell = random.choice(random.choice(self.cells))

            # create and start the fire
            currentFire = Fire(self, fireSize)
            burningCells.extend(currentFire.start(startingCell))
        
        # regenerate empty cells
        i = 0
        while i < len(self.emptyCells):
            if random.random() < self.regenerationProbability:
                self.emptyCells[i].empty = False
                del self.emptyCells[i]
            else:
                i += 1
        
        # reset the burning flag for burned cells
        for targetCell in burningCells:
            targetCell.burning = False
            targetCell.empty = True
            
        self.emptyCells.extend(burningCells)
        self.display.update()

class Cell(object):
    def __init__(self, x, y):
        self.burning = False
        self.empty = False
        self.neighbors = []
        self.x = x
        self.y = y
        self.fireVisited = False
    
    def can_burn(self):
        return not self.burning and not self.empty
    
    def coord_str(self):
        return "(" + str(self.x) + "," + str(self.y) + ")"
        

class GridDisplay(object):
    def __init__(self, grid, dimensions, displayCellSize = 5):
        self.grid = grid
        self.dimensions = dimensions
        
        ### initialize the display
        self.displayWindow = gridTk(int(displayCellSize), self.dimensions)
        self.update()

    def update(self):
        for col in self.grid.cells:
            for cell in col:
                self.update_cell(cell)
        self.displayWindow.tkupdate()
    
    def update_cell(self, updateCell):
        yl = self.dimensions[1] - updateCell.y
        xl = updateCell.x + 1
        color = self.colors(updateCell)
        self.displayWindow.update((xl,yl), color)
        
    def colors(self, cell):
        if cell.burning:
            col = "#ff9900"
        elif cell.empty:
            col = "#809980"
        else:
            col = "#194719"
        return col


class gridTk(object):
    def __init__(self, cellSize, gridDim):

        self.done = 0   # exit flag set to False

        self.cells = []
        self.row = []
        if( (type(gridDim) is int) or (len(gridDim) == 1) ):    # if we only get passed one value, square array
            self.gridSize = (gridDim, gridDim)
        else:
            self.gridSize = gridDim
            self.cellSize = cellSize
        pixels = (gridDim[0] * self.cellSize, gridDim[1] * self.cellSize)
        self.a = {}

        # set up Tk

        self.tk = Tkinter.Tk()
        self.tk.wm_geometry(str(pixels[0]) + 'x' + str(pixels[1]) + '+20+40')
        self.tk.bind("<Return>", self.finish)
        self.canvas = Tkinter.Canvas(self.tk, width=pixels[0], height=pixels[1])
        self.canvas.pack()

        y=0

        for i in range(0, self.gridSize[0] * self.cellSize, self.cellSize):
            x=0
            y += 1
            for j in range(0, self.gridSize[1] * self.cellSize, self.cellSize):
                x += 1
                self.a[(y,x)] = \
                    self.canvas.create_rectangle(i, j, i + self.cellSize, \
                                                 j + self.cellSize)#, fill='gray')

        self.tk.update()

    def finish(self, e):
        self.done = 1

    def update(self, cellRef, color):
        # put your update code here
        ## cellRef is 0 indexed, so we add 1 for the indexing of the display
        self.canvas.itemconfigure(self.a[cellRef], fill=color, outline=color)
        
    def tkupdate(self):
        self.tk.update()


def rpois(L):
    k = 0
    prod = random.random()
    while prod >= math.exp(-L):
        prod *= random.random()
        k += 1
    result = k
    return(result)


def rzip(pi, L):
    """ return a random value drawn from a zero inflated poisson with
    zero probability pi and poisson mean L
    """
    if random.random() < pi:
        result = rpois(L)
    else:
        result = 0
    return result
    
def calculate_offsets(d):
    """ for a given distance d, return offsets from a target cell to get neighbors
    e.g. (1,-1) means a cell that is diagonally up and to the left of the target cell is a neighbor
    """
    offsets = []
    dx = int(math.ceil(d))
    dy = 0
    while dx >= 0:
        while dist( (0,0), (dx,dy) ) <= d:
            if (not (dx == 0 and dy == 0)): offsets.append( (dx,dy) )
            if dx > 0 and dy > 0: offsets.append( (-dx, -dy) )
            if dx > 0: offsets.append( (-dx,dy) )
            if dy > 0: offsets.append( (dx, -dy) )
            dy = dy + 1
        dy = 0
        dx = dx - 1
    return offsets
    
    
def dist(c1, c2):
    return math.sqrt(math.pow(c1[0] - c2[0], 2) + math.pow(c1[1] - c2[1], 2))
    
    
if __name__ == '__main__':
    main()