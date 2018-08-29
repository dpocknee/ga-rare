/*noise-generator.exe output_file_name random_seed maximum_change
Generates a 1 second file of noise and the maximum and minimum values can be specified. 
This program takes four arguments when called via the command line:
- [1] output file name
- [2] length of file in samples (44100 = 1 second)
- [2] random seed
- [3] maximum change in integer values.  (maximum 32652)
        This is the maximum absolute value of any integer
        e.g. 8 would allow an integer to change by up to +8 or -8.  

To generate a file of pure white noise, write: 
noise-generator.exe outputfile.wav 734 32652

To generate a blank file, write: 
noise-generator.exe outputfile.wav 523 0 */


#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "make_wav.h"
#include <time.h>

/* The write_little_endian and write_wav functions as well as make_wav.h are taken from 
 * Kevin Karplus's make_wav.c code
 * Fri Jun 18 16:36:23 PDT 2010 
 * Creative Commons license Attribution-NonCommercial
 *  http://creativecommons.org/licenses/by-nc/3.0/
  */

void write_little_endian(unsigned int word, int num_bytes, FILE *wav_file)
{
    unsigned buf;
    while(num_bytes>0)
    {   buf = word & 0xff;
        fwrite(&buf, 1,1, wav_file);
        num_bytes--;
    word >>= 8;
    }
}


void write_wav(char * filename, unsigned long num_samples, short int * data, int s_rate)
{
    FILE* wav_file;
    unsigned int sample_rate;
    unsigned int num_channels;
    unsigned int bytes_per_sample;
    unsigned int byte_rate;
    unsigned long i;    /* counter for samples */

    num_channels = 1;   /* monoaural */
    bytes_per_sample = 2;

    if (s_rate<=0) sample_rate = 44100;
    else sample_rate = (unsigned int) s_rate;

    byte_rate = sample_rate*num_channels*bytes_per_sample;

    wav_file = fopen(filename, "wb");
    assert(wav_file);   /* make sure it opened */

    /* write RIFF header */
    fwrite("RIFF", 1, 4, wav_file);
    write_little_endian(36 + bytes_per_sample* num_samples*num_channels, 4, wav_file);
    fwrite("WAVE", 1, 4, wav_file);

    /* write fmt  subchunk */
    fwrite("fmt ", 1, 4, wav_file);
    write_little_endian(16, 4, wav_file);   /* SubChunk1Size is 16 */
    write_little_endian(1, 2, wav_file);    /* PCM is format 1 */
    write_little_endian(num_channels, 2, wav_file);
    write_little_endian(sample_rate, 4, wav_file);
    write_little_endian(byte_rate, 4, wav_file);
    write_little_endian(num_channels*bytes_per_sample, 2, wav_file);  /* block align */
    write_little_endian(8*bytes_per_sample, 2, wav_file);  /* bits/sample */

    /* write data subchunk */
    fwrite("data", 1, 4, wav_file);
    write_little_endian(bytes_per_sample* num_samples*num_channels, 4, wav_file);
    for (i=0; i< num_samples; i++)
    {   write_little_endian((unsigned int)(data[i]),bytes_per_sample, wav_file);
    }

    fclose(wav_file);
}


/* My code*/

int rnd(int range);
void seedrnd(void);
int randshuffler(int pool, int total);

#define S_RATE  (44100)
#define BitRate (65536)
#define HEADER_SIZE (22) // this is the header size (actually 44 bytes, but in 22 8 byte blocks)

int main(int argc, char* argv[])
{
     int k,l,randseed,random_change_var, random_change, newval;
     char outname[40];

    int buf_size = atoi(argv[2]);
    int file_size = (buf_size+HEADER_SIZE);
    short int newbuffer[buf_size];

     sprintf(outname, "%s",argv[1]);
     FILE * outfile = fopen(outname, "wb");

     if(argc<5) {printf("Error: Not enough variables! (%i)", argc);}
     else if (argc>5)  { printf("Error: Too many variables (%i)!", argc); }
     else
     {
        randseed = atoi(argv[3]);
        random_change_var = atoi(argv[4]);
        srand(randseed);

         for(k=HEADER_SIZE;k<file_size;k++)
         {
              l = k-HEADER_SIZE;
            //The new value moved up or down a random amount:
            if(random_change_var == 0) { 
                newval = 0;
            }
            else {
                random_change = randshuffler(7,(2*random_change_var))-random_change_var;    
                newval = 0 + random_change;
            }  
            newbuffer[l] = (short int)(newval);   
         }
         write_wav(outname, buf_size, newbuffer, S_RATE);
         printf("\nWrote: %s\n",outname);
     }
     return(0);
}

/*creates an entropy pool to make the randomness more random*/
int randshuffler(int pool, int total)
{
     int z, randbuff[pool], randmid, randout;

     for(z=0;z<pool;z++)
     {
          randmid = rnd(total);
          randbuff[z] = (int)(randmid);
     }
     randout = randbuff[rnd(pool)];
     return(randout);
}

/*generates random number in the range given
It is designed for  */
int rnd(int range)
{
     int r1, r2, rtotal;
     r1=rand();
     r2=rand();
     rtotal=(r1+r2)%range;
     return(rtotal);
}

/* seeds the random number using the time*/
void seedrnd(void)
{
     srand((unsigned)time(NULL));
}
