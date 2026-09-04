"""
"""
import tkinter as tk
from   tkinter import ttk
import matplotlib.pyplot as plt
import matplotlib.backends.backend_tkagg as tkagg
import time

from inline_python import AsyncCancelled

class App():
    def __init__(self):
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
            self.root.bind('<<PLOT>>', self.handle_plot)
            self.root.bind('<<BONK>>', self.handle_bonk)
        except Exception:
            # We need to destroy root window if any exception during
            # construction is raised. Otherwise it will get shown
            self.root.destroy()
            raise

    def handle_plot(self, evt):
        # Break event loop and return control to main program
        self.exit_reason = evt
        self.root.quit()

    def handle_bonk(self, evt):
        # Drop from python interpreter and do nothing. This is needed to
        # be able catch async exception
        try:
            time.sleep(0)
        except AsyncCancelled:
            self.root.quit()

    def mainloop(self):
        self.exit_reason = None
        self.fig.canvas.draw()
        self.root.mainloop()
