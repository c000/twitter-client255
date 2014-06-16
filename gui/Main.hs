import Imports
import Constructors

main = do
    initGUI
    mainWindow <- constructMainWindow
    widgetShowAll mainWindow
    mainGUI
