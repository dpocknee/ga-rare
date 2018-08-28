/*This code generates 1 second noise as a mono  16bit 44100Hz wav file*/
/* when called from the command line, it needs to have one argument:
the output filename, missing the file extension (this is added automatically)*/

/* make_wav.c
 * Creates a WAV file from an array of ints.
 * Output is monophonic, signed 16-bit samples
 * copyright
 * Fri Jun 18 16:36:23 PDT 2010 Kevin Karplus
 * Creative Commons license Attribution-NonCommercial
 *  http://creativecommons.org/licenses/by-nc/3.0/
 */

#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "make_wav.h"
#include <time.h>

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

#define S_RATE  (44100)
#define BUF_SIZE (S_RATE*1) /* x second buffer */
#define BitRate (65536)

short int buffer[BUF_SIZE]; /*This holds all the PCM values */

int rnd(int range);
void seedrnd(void);
int randshuffler(int pool);

/*This main function takes one argument when called via the command line:
the target filename*/
int main(int argc, char* argv[])
{
     int i;
     int randval, seed;
     char filename[40];
     char type[4]=".wav";

     if(argc==1)
     {
          printf("Error: No target filename specified");
     }
     else
     {
          seed = atoi(argv[2]);
          srand(seed);

          for(i=0;i<BUF_SIZE;i++)
          {
               randval = randshuffler(30)-(BitRate*0.5);
               buffer[i] = (short int)(randval);
          }
          sprintf(filename, "%s.wav",argv[1]);
          printf("Wrote: %s\n",filename);
          write_wav(filename, BUF_SIZE, buffer, S_RATE);
     }

     return(0);
}

/*creates an entropy pool to make the randomness more random*/
int randshuffler(int pool)
{
     int z;
     int randbuff[pool];
     int randmid;
     int randout;


     for(z=0;z<pool;z++)
     {
          randmid = rnd(BitRate);
          randbuff[z] = (int)(randmid);
     }
     randout = randbuff[rnd(pool)];
     return(randout);
}

/*generates random number in the range given
It is designed for  */
int rnd(int range)
{
     int r1;
     int r2;
     int rtotal;

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
