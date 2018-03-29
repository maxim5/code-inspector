using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

class Algorithm
{
    double contextswitching = 0;
    public double Average_TA_time = 0;
    public double Average_WTA_time = 0;
    public double standard_deviaton = 0;
    int lines = 0;
    Process NowUsing;

    public Process[] Initialize(string fileName)
    {
        TextReader tr = new StreamReader(fileName);
        string text = tr.ReadLine();
        lines = Int32.Parse(text);
        Process[] Processes = new Process[lines];
        for (int i = 0; i < lines; i++)
        {
            text = tr.ReadLine();
            string[] temp = text.Split(' ');
            Processes[i] = new Process();
            Processes[i].Processno = i + 1;
            Processes[i].ArrivalTime = Int32.Parse(temp[1]);
            Processes[i].RunTime = Int32.Parse(temp[2]);
            Processes[i].Priority = Int32.Parse(temp[3]);
            Processes[i].EndingTime = -1;
            Processes[i].RemainingTime = Processes[i].RunTime;
        }
        return Processes;
    }
    //this method returns the highest priority process according to the algorithm Used 
    private Process highestpriority(int lines, ref Process[] Processes, int alg, int time)
    {
        Process next = new Process();
        if (alg == 0)
        {
            next.ArrivalTime = 999;
            for (int i = 0; i < lines; i++)
            {
                if (Processes[i].ArrivalTime < next.ArrivalTime && Processes[i].EndingTime == -1 && Processes[i].ArrivalTime <= time)
                {
                    next = Processes[i];
                }
            }
            if (next.ArrivalTime == 999)
            {
                return null;
            }
            else
                return next;
        }
        else if (alg == 1)
        {
            if (NowUsing != null)
            {
                next = NowUsing;
            }
            else
            {
                next.RunTime = 999;
            }

            for (int i = 0; i < lines; i++)
            {
                if (Processes[i].RunTime < next.RunTime && Processes[i].EndingTime == -1 && Processes[i].ArrivalTime <= time)
                {
                    next = Processes[i];
                }else
                    if (Processes[i].RunTime == next.RunTime && Processes[i].ArrivalTime < next.ArrivalTime && Processes[i].EndingTime == -1 && Processes[i].ArrivalTime <= time)
                    {
                        next = Processes[i];
                    }
            }
            if (next.RunTime == 999)
            {
                return null;
            }
            else
                return next;
        }
        else if (alg == 2)
        {
            if (NowUsing != null )
            {
                next = NowUsing;
            }
            else
            {
                next.Priority = 999;
            }
            for (int i = 0; i < lines; i++)
            {
                if (Processes[i].Priority < next.Priority && Processes[i].EndingTime == -1 && Processes[i].ArrivalTime <= time)
                {
                    next = Processes[i];
                }
                else
                    if (Processes[i].Priority < next.Priority && Processes[i].ArrivalTime < next.ArrivalTime && Processes[i].EndingTime == -1 && Processes[i].ArrivalTime <= time)
                    {
                        next = Processes[i];
                    }
            }
            if (next.Priority == 999)
            {
                return null;
            }
            else
                return next;
        }
        else if (alg == 3)
        {
            if (NowUsing != null)
            {
                next = NowUsing;
            }
            else
            {
                next.WeightedPriority = 999;
            }
            for (int i = 0; i < lines; i++)
            {
                if (Processes[i].WeightedPriority < next.WeightedPriority && Processes[i].EndingTime == -1 && Processes[i].ArrivalTime <= time)
                {
                    next = Processes[i];
                }
                else
                    if (Processes[i].WeightedPriority < next.WeightedPriority && Processes[i].ArrivalTime < next.ArrivalTime && Processes[i].EndingTime == -1 && Processes[i].ArrivalTime <= time)
                    {
                        next = Processes[i];
                    }
            }
            if (next.WeightedPriority == 999)
            {
                return null;
            }
            else
                return next;
        }
        else
            return next;


    }
    private TimeSlot[] preemptiveQuantized(int alg, ref Process[] Processes,int quantum,double SwitchingTime)
    {
        double x = 0;
        for (int j = 0; j < Processes.Length; j++)
        {
            x += Processes[j].RunTime * 1.5;
        }
        TimeSlot[] TimeLine = new TimeSlot[Processes.Length + (int)x];
        int TimeP = 0;
        int counter = 0;
        int time = 0;

        TimeLine[TimeP] = new TimeSlot();
        TimeLine[TimeP].length = 1;
        TimeLine[TimeP].process = new Process();
        TimeLine[TimeP].process.Processno = -1;
        TimeP++;
        time++;

        while (counter < Processes.Length)
        {
            Process next = highestpriority(Processes.Length, ref Processes, alg, time);
            if (next != null)
            {
                
                
                if (next.RemainingTime > 0 && next.ArrivalTime <= time)
                {
                    if (NowUsing != next && NowUsing != null)
                    {
                        TimeLine[TimeP] = new TimeSlot();
                        TimeLine[TimeP].length = SwitchingTime;
                        TimeP++;
                    }
                    
                    TimeLine[TimeP] = new TimeSlot();
                    TimeLine[TimeP].process = next;
                    if (next.RemainingTime <= quantum)
                    {
                        time += next.RemainingTime;
                        TimeLine[TimeP].length = next.RemainingTime;
                        next.RemainingTime = 0;
                        next.EndingTime = time;
                        counter++;
                        next = null;
                        NowUsing = null;
                    }
                    else
                    {
                        time += quantum;
                        next.RemainingTime -= quantum;
                        TimeLine[TimeP].length = quantum;
                        NowUsing = next;
                    }
                    TimeP++;
                }
            }
            else
            {
                NowUsing = null;
                TimeLine[TimeP] = new TimeSlot();
                TimeLine[TimeP].process = new Process();
                TimeLine[TimeP].process.Processno = -1;
                TimeLine[TimeP].length = 1;
                time += 1;
                TimeP++;

            }
        }
        return TimeLine;
    }
    public void Sort(ref Process [] Processes)
    {

        for (int pass = 1; pass < Processes.Length; pass++)
            for (int i = 0; i < Processes.Length - 1; i++)
                if (Processes[i].ArrivalTime > Processes[i+1].ArrivalTime)
            swap( i, i+1 ,ref Processes);
    }
    public void swap(int pro1 , int pro2,ref Process [] Processes)
    {
        Process temp = Processes[pro1];
        Processes[pro1]=Processes[pro2];
        Processes[pro2] = temp;
    }
    private TimeSlot[] RoundRobinQ( ref Process[] Processes,int quantum,double SwitchingTime)
    {
        Sort(ref Processes);
        double x = 0;
        for (int j = 0; j < Processes.Length; j++)
        {
            x += Processes[j].RunTime * 1.5;
        }
        TimeSlot[] TimeLine = new TimeSlot[Processes.Length + (int)x];
        int TimeP = 0;
        int counter = 0;
        int i = 0;
        int time = 0;
        int lasttime = 0;
        int lastPro = 23;
        TimeLine[TimeP] = new TimeSlot();
        TimeLine[TimeP].length = 1;
        TimeLine[TimeP].process = new Process();
        TimeLine[TimeP].process.Processno = -1;
        TimeP++;
        time++;

        while (counter < Processes.Length)
        {

            if (i == lines)
            {
                if (lasttime == time)
                {
                    time++;
                    TimeLine[TimeP] = new TimeSlot();
                    TimeLine[TimeP].length = 1;
                    TimeLine[TimeP].process = new Process();
                    TimeLine[TimeP].process.Processno = -1;
                    TimeP++;
                }
                i = 0;
                lasttime = time;
            }
            if (lastPro == Processes[i].Processno && Processes[i].EndingTime != -1)
            {
                TimeLine[TimeP] = new TimeSlot();
                TimeLine[TimeP].length = 1;
                TimeLine[TimeP].process = new Process();
                TimeLine[TimeP].process.Processno = -1;
                TimeP++;
                time += 1;
                NowUsing = null;
            }
            else if (Processes[i].RemainingTime > 0 && Processes[i].ArrivalTime <= time)
            {
                if (NowUsing != Processes[i] && NowUsing != null)
                {
                    TimeLine[TimeP] = new TimeSlot();
                    TimeLine[TimeP].length = SwitchingTime;
                    TimeP++;
                }
                
                NowUsing = Processes[i];
                lastPro = Processes[i].Processno;
                TimeLine[TimeP] = new TimeSlot();
                TimeLine[TimeP].process = Processes[i];
                if (Processes[i].RemainingTime <= quantum)
                {
                    time += Processes[i].RemainingTime;
                    TimeLine[TimeP].length = Processes[i].RemainingTime;
                    Processes[i].RemainingTime = 0;
                    Processes[i].EndingTime = time;
                    counter++;
                }
                else
                {
                    time += quantum;
                    Processes[i].RemainingTime -= quantum;
                    TimeLine[TimeP].length = quantum;
                    //TimeP++;
                    //TimeLine[TimeP] = new TimeSlot();
                    //TimeLine[TimeP].length = SwitchingTime;

                }

                TimeP++;
            }
            i++;
        }
        return TimeLine;
    }
    private TimeSlot[] Nonpreemptive(int alg, ref Process[] Processes)
    {
        double x = 0;
        for (int j = 0; j < Processes.Length; j++)
        {
            x += Processes[j].RunTime * 1.5;
        }
        TimeSlot[] TimeLine = new TimeSlot[Processes.Length + (int)x];
        int time = 0;
        int TimeP = 0;
        for (int i = 0; i < Processes.Length; i++)
        {
            Process next = highestpriority(Processes.Length, ref Processes, alg, time);
            if (next != null)
            {
                time += next.RunTime;
                next.EndingTime = time;
                TimeLine[TimeP] = new TimeSlot();
                TimeLine[TimeP].process = next;
                TimeLine[TimeP].length = next.RunTime;
                TimeP++;
                TimeLine[TimeP] = new TimeSlot();
                TimeLine[TimeP].process = null;
                TimeLine[TimeP].length = contextswitching;
                TimeP++;

            }
            else
            {
                time++;
                TimeLine[TimeP] = new TimeSlot();
                TimeLine[TimeP].process = new Process();
                TimeLine[TimeP].process.Processno = -1;
                TimeLine[TimeP].length = 1;
                TimeP++;
                i--;
            }

        }
        TimeLine[TimeP] = new TimeSlot();
        TimeLine[TimeP].length = -1;
        TimeLine[TimeP].process = null;

        return TimeLine;
    }
    public TimeSlot[] NonPreemptiveShortestJob(ref Process[] Processes)
    {
       return Nonpreemptive(1,  ref Processes);
    }
    public TimeSlot[] NonPreemptiveHighestPriority(ref Process[] Processes)
    {
        return Nonpreemptive(2, ref Processes);
    }
    public TimeSlot[] NonPreemptiveArrivalTime(ref Process[] Processes)
    {
        return Nonpreemptive(0, ref Processes);
    }
    public TimeSlot[] PreemptiveShortestJob(ref Process[] Processes,int quantum,double SwitchingTime)
    {
        return preemptiveQuantized(1, ref Processes, quantum,SwitchingTime);
    }
    public TimeSlot[] PreemptiveHighestPriority(ref Process[] Processes,int quantum,double SwitchingTime)
    {
        return preemptiveQuantized(2, ref Processes, quantum,SwitchingTime);
    }
    public TimeSlot[] RoundRobin(ref Process[] Processes, int quantum,double SwitchingTime)
    {
        return RoundRobinQ(ref Processes,quantum,SwitchingTime);
    }
    public void Performance_Evaluation(ref Process[] processes)
    {
        float b = 1;
        float c;
        for (int i = 0; i < lines; i++)
        {
            processes[i].TA_time = processes[i].EndingTime - processes[i].ArrivalTime;
            b *= processes[i].RunTime;
            processes[i].WTA_time = processes[i].TA_time / processes[i].RunTime;
            Average_TA_time += processes[i].TA_time;
        }
        for (int i = 0; i < lines; i++) 
        {
            c = b / processes[i].RunTime;
            processes[i].WTA_time = (c * processes[i].TA_time)/b;
            Average_WTA_time += processes[i].WTA_time;
        }
        Average_TA_time = Average_TA_time / lines;
        Average_WTA_time = Average_WTA_time / lines;
        for (int k = 0; k < lines; k++)
        {
            standard_deviaton += ((processes[k].WTA_time - Average_WTA_time) * (processes[k].WTA_time - Average_WTA_time)) / lines;
        }
        standard_deviaton = Math.Sqrt(standard_deviaton);
    }
    public void weightedPriorityCalc(ref Process[] processes, double Weight_arr, double Weight_run, double Weight_Pri)
    {
        for (int i = 0; i < processes.Length; i++)
        {
            processes[i].WeightedPriority = (processes[i].ArrivalTime * Weight_arr)+(processes[i].Priority * Weight_Pri)+(processes[i].RunTime * Weight_run);
        }
    }
    public TimeSlot[] NonPreemptiveWeightedPriority(ref Process[] Processes)
    {
        return Nonpreemptive(3, ref Processes);
    }

}
