"""
"""
import os
import tkinter as tk
from   tkinter import ttk
import matplotlib.pyplot as plt
import matplotlib.backends.backend_tkagg as tkagg

class App():
    def __init__(self, fd):
        # Tk widgets
        self.root        = tk.Tk()
        self.exit_reason = None
        # Stop if windown is destroyed
        self.root.protocol("WM_DELETE_WINDOW", lambda: self.root.quit())
        try:
            # I have no idea why but without this frame (or other
            # widget) canvas fill whole root widget diding navbar
            top_frame = ttk.Frame(self.root)
            top_frame.pack(side='top', expand=1, fill='x')
            #--
            self.fig    = plt.figure()
            self.canvas = tkagg.FigureCanvasTkAgg(self.fig, self.root)
            self.canvas.get_tk_widget().pack(side='top', fill=tk.BOTH, expand=True)
            #--
            frame = ttk.Frame(self.root)
            frame.pack(side='top', fill="x", expand=1)
            nav = tkagg.NavigationToolbar2Tk(self.fig.canvas, frame)
            nav.pack(side="left", expand=0, padx=10, pady=10)
            # Set up handlers
            self.root.createfilehandler(fd, tk.READABLE, self.handlerRead)
        except Exception:
            # We need to destroy root window if any exception during
            # construction is raised. Otherwise it will get shown
            self.root.destroy()
            raise

    def handlerRead(self, file, mask):
        cmd = os.read(file, 1024)
        self.exit_reason = cmd
        match cmd:
            case b'q':
                try:
                    self.root.quit()
                    self.root.destroy()
                except tk.TclError:
                    pass
            case b's':
                self.root.quit()
            case _:    raise Exception("Unknown command" + str(cmd))

    def mainloop(self):
        self.exit_reason = None
        self.fig.canvas.draw()
        self.root.mainloop()

    def done(self):
        try:
            self.root.destroy()
        except tk.TclError:
            pass
