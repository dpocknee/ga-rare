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
#define HEADER_SIZE (22) // this is the header size (actually 44 bytes, but in 22 8 byte blocks)
#define FILE_SIZE (BUF_SIZE+HEADER_SIZE)


int rnd(int range);
void seedrnd(void);
int randshuffler(int pool, int total);

/*This main function takes four arguments when called via the command line:
- [1] input file 1  
- [2] input file 2
- [3] output file 1 name
- [4] output file 2 name
- [5] random seed
- [6] minimum percentage of file to copy (int)
- [7] maxmum percentage of file to copy (int)*/

int main(int argc, char* argv[])
{
     // This has been optimized for speed.
     int i,k,l,buff_find;
     int start_pos, end_pos, pos_diff, size, start_seek;
     char inname1[40], inname2[40], outname1[40], outname2[40];
     short int bufferin1[FILE_SIZE],bufferin2[FILE_SIZE],newbuffer1[BUF_SIZE],newbuffer2[BUF_SIZE];
     int pos,randseed;
     float minpercent, maxpercent;
     int percentval, chance_of_change,change_chance,random_change_var, random_change;
     int top_value, top_distance, bottom_value, bottom_distance, lowest_random, newval;
     short int currentval;


     if(argc<8)
     {
          printf("Error: Not enough variables! (%i)", argc);
     }
     else if (argc>8)
     {
          printf("Error: Too many variables (%i)!", argc);
     }
     else
     {

         sprintf(inname1, "%s",argv[1]);
         sprintf(inname2, "%s",argv[2]);
         sprintf(outname1, "%s",argv[3]);
         sprintf(outname2, "%s",argv[4]);

         FILE * infile1 = fopen(inname1, "rb");
         FILE * infile2 = fopen(inname2, "rb");
         FILE * outfile1 = fopen(outname1, "wb");
         FILE * outfile2 = fopen(outname2, "wb");

         // Random seeding
          randseed = atoi(argv[5]);
          srand(randseed);

        // Copy all of file 1 into bufferin1
         fread(bufferin1, sizeof(short int), FILE_SIZE,infile1);

         // Copy all of file 2 into bufferin2
         fread(bufferin2, sizeof(short int), FILE_SIZE,infile2);


         //Specification of the range to be swapped
         // This is the section of the files that will be swapped
        minpercent = BUF_SIZE*(atof(argv[6])/100);
        maxpercent = BUF_SIZE*(atof(argv[7])/100);
        percentval = (int)(maxpercent-minpercent);
        printf("minpercent: %f\n",minpercent);
        printf("maxpercent: %f\n",maxpercent);
         
        size = minpercent + randshuffler(30,percentval);
         start_pos = randshuffler(30,(BUF_SIZE-size));
         end_pos = start_pos + size;



         //  Build Output File 1
         for(k=HEADER_SIZE;k<FILE_SIZE;k++)
         {
              l = k-HEADER_SIZE;
              if (k<start_pos || k>end_pos)
              {
                    newbuffer1[l] = (short int)(bufferin1[k]);
              }
              else
              {
                   newbuffer1[l] = (short int)(bufferin2[k]);
              }
         }

         // Build Output File 2
        for(k=HEADER_SIZE;k<FILE_SIZE;k++)
         {
              l = k-HEADER_SIZE;
              if (k<start_pos || k>end_pos)
              {
                    newbuffer2[l] = (short int)(bufferin2[k]);
              }
              else
              {
                   newbuffer2[l] = (short int)(bufferin1[k]);
              }
         }

         write_wav(outname1, BUF_SIZE, newbuffer1, S_RATE);
         printf("\nWrote: %s\n",outname1);
         write_wav(outname2, BUF_SIZE, newbuffer2, S_RATE);
         printf("\nWrote: %s\n",outname2);

          // cleanup
          if (infile1) { fclose(infile1); }
          if (infile2) { fclose(infile2); }
     }

     return(0);
}

/*creates an entropy pool to make the randomness more random*/
int randshuffler(int pool, int total)
{
     int z;
     int randbuff[pool];
     int randmid;
     int randout;

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
