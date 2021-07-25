/*
 * Copyright (c) 2017, 2021 by Matthew Jacob
 * All rights reserved.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of The version 2 GNU General Public License as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 * Matthew Jacob
 * Feral Software
 * 253 Lowell Street
 * Redwood City, CA 94062
 * USA
 *
 * gpl at feralsw com
 */
/*
 * Sample driver to create a block device out of memory pages
 */

#include <linux/module.h>
#include <linux/version.h>
#include <linux/blkdev.h>
#include <linux/hdreg.h>
#include <linux/slab.h>

#if LINUX_VERSION_CODE < KERNEL_VERSION(3, 14, 0)
#define BIO_VEC_T       struct bio_vec *
#define BIOVEC(b)       b
#define BVEC_ITER_T     int
#define BIO_SECTOR(b)   (b)->bi_sector
#define BIO_SIZE(b)     (b)->bi_size
#define BIO_IDX(b)      (b)->bi_idx
#else
#define BIO_VEC_T       struct bio_vec
#define BIOVEC(b)       (&b)
#define BVEC_ITER_T     struct bvec_iter
#define BIO_SECTOR(b)   (b)->bi_iter.bi_sector
#define BIO_SIZE(b)     (b)->bi_iter.bi_size
#define BIO_IDX(b)      (b)->bi_iter.bi_idx
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(4, 3, 0)
#define BIO_ENDIO   bio_endio
#else
#define BIO_ENDIO(bio, err) do {    \
    bio->bi_error = err;            \
    bio_endio(bio);                 \
} while (0)
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3, 2, 0)
#define MRQTYPE int
#define MRQRETV return (0)
#elif LINUX_VERSION_CODE < KERNEL_VERSION(4, 4, 0)
#define MRQTYPE void
#define MRQRETV return
#else
#define MRQTYPE blk_qc_t
#define MRQRETV return (BLK_QC_T_NONE)
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(4, 8, 0) && defined(RHEL)
#define BIO_HAS_FLUSH(b)    bio_rw_flagged((b), BIO_RW_FLUSH)
#define BIO_HAS_DISCARD(b)  bio_rw_flagged((b), BIO_RW_DISCARD)
#define BIO_HAS_FUA(b)      bio_rw_flagged((b), BIO_RW_FUA)
#define BIO_HAS_FAILFAST(b) bio_rw_flagged((b), BIO_RW_FAILFAST_DEV)
#define BIO_HAS_RA(b)       bio_rw_flagged((b), BIO_RW_AHEAD)
#define BIO_CLEAR_FLUSH(b)  (b)->bi_rw &= ~BIO_FLUSH
#elif LINUX_VERSION_CODE < KERNEL_VERSION(4, 8, 0)
#define BIO_HAS_FLUSH(b)    ((b)->bi_rw & REQ_FLUSH)
#define BIO_HAS_DISCARD(b)  ((b)->bi_rw & REQ_DISCARD)
#define BIO_HAS_FUA(b)      ((b)->bi_rw & REQ_FUA)
#define BIO_HAS_FAILFAST(b) ((b)->bi_rw & REQ_FAILFAST_DEV)
#define BIO_HAS_RA(b)       ((b)->bi_rw & REQ_RAHEAD)
#define BIO_CLEAR_FLUSH(b)  (b)->bi_rw &= ~REQ_FLUSH
#else
#define BIO_HAS_FLUSH(b)    (bio_op(b) == REQ_OP_FLUSH)
#define BIO_HAS_DISCARD(b)  (bio_op(b) == REQ_OP_DISCARD)
#define BIO_HAS_FUA(b)      ((b)->bi_opf & REQ_FUA)
#define BIO_HAS_FAILFAST(b) ((b)->bi_opf & REQ_FAILFAST_DEV)
#define BIO_HAS_RA(b)       ((b)->bi_opf & REQ_RAHEAD)
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3, 10, 0)
#define RTYP    int
#define RRET    return (0)
#else
#define RTYP    void
#define RRET    return;
#endif

#define MODULE_NAME "mdev"

#define MDEV_ORDER  8
#define ROW_SIZE    (PAGE_SIZE << MDEV_ORDER)
#define DISK_SIZE   (npgl * ROW_SIZE)

struct mdev {
    struct request_queue *queue;
    struct gendisk *disk;
};

static struct class *mdev_class;
static int mdev_cmajor;
static int mdev_bmajor;
static struct page **pgl;
static struct device *mdev_device;
static struct mdev *mdroot;

static int npgl = 256;
module_param(npgl, int, 0444);


static int
mdev_fopen(struct inode *inode, struct file *file)
{
    file->f_pos = 0;
    inode->i_size = PAGE_SIZE << MDEV_ORDER;
    return (0);
}

static int
mdev_fclose(struct inode *inode, struct file *file)
{
    return (0);
}

static ssize_t
mdev_fread(struct file *file, char __user * buffer, size_t count, loff_t *ppos)
{
    unsigned char *data;
    loff_t off, orig_off = *ppos;
    unsigned int idx;
    ssize_t retv;
    size_t xf, curcnt = count;

    retv = DISK_SIZE - *ppos;
    if (curcnt > retv) {
        curcnt = retv;
    } else {
        retv = curcnt;
    }

    while (curcnt > 0) {
        idx = *ppos / ROW_SIZE;
        off = *ppos % ROW_SIZE;

        xf = curcnt;
        if (off + curcnt > ROW_SIZE)
            xf = ROW_SIZE - off;

        data = page_address(pgl[idx]);

        if (copy_to_user(buffer, data + off, xf)) {
            *ppos = orig_off;
            return (-EFAULT);
        }

        *ppos += xf;
        curcnt -= xf;
    }

    return (retv);
}

static ssize_t
mdev_fwrite(struct file *file, const char __user * buffer, size_t count, loff_t *ppos)
{
    unsigned char *data;
    loff_t off, orig_off = *ppos;
    unsigned int idx;
    ssize_t retv;
    size_t xf, curcnt = count;

    retv = DISK_SIZE - *ppos;
    if (curcnt > retv) {
        curcnt = retv;
    } else {
        retv = curcnt;
    }

    while (curcnt > 0) {
        idx = *ppos / ROW_SIZE;
        off = *ppos % ROW_SIZE;

        xf = curcnt;
        if (off + curcnt > ROW_SIZE)
            xf = ROW_SIZE - off;

        data = page_address(pgl[idx]);

        if (copy_from_user(data + off, buffer, xf)) {
            *ppos = orig_off;
            return (-EFAULT);
        }

        *ppos += xf;
        curcnt -= xf;
    }

    return (retv);
}

static loff_t
mdev_fseek(struct file *file, loff_t off, int orig)
{
    if (orig != SEEK_CUR && orig != SEEK_SET)
        return ((loff_t)-EINVAL);

    if (orig == SEEK_CUR)
        off += file->f_pos;
    file->f_pos = off;

    return (off);
}

const struct file_operations mdev_fops = {
    .owner          = THIS_MODULE,
    .open           = mdev_fopen,
    .release        = mdev_fclose,
    .read           = mdev_fread,
    .write          = mdev_fwrite,
    .llseek         = mdev_fseek,
};

static int
mdev_bd_open(struct block_device *bdev, fmode_t mode)
{
    struct mdev *md;

    md = bdev->bd_disk->private_data;
    if (!md)
        return (-ENXIO);

    return (0);
}

static RTYP
mdev_bd_close(struct gendisk *disk, fmode_t mode)
{
    RRET;
}

static int
mdev_bd_getgeo(struct block_device *bd, struct hd_geometry *geo)
{
    geo->heads = 1 << 6;
    geo->sectors = 1 << 5;
    geo->cylinders = get_capacity(bd->bd_disk) >> 11;
    return (0);
}

static const struct block_device_operations mdev_bdfops = {
    .owner      = THIS_MODULE,
    .open       = mdev_bd_open,
    .release    = mdev_bd_close,
    .getgeo     = mdev_bd_getgeo,
};

static void
mdev_start_io_acct(struct bio *bio)
{
	struct gendisk *disk = bio->bi_bdev->bd_disk;
	if (blk_queue_io_stat(disk->queue)) {
		const int rw = bio_data_dir(bio);
		int cpu = part_stat_lock();
		part_round_stats(cpu, &disk->part0);
		part_stat_inc(cpu, &disk->part0, ios[rw]);
		part_stat_add(cpu, &disk->part0, sectors[rw],
							bio_sectors(bio));
		part_inc_in_flight(&disk->part0, rw);
		part_stat_unlock();
	}
}

static void
mdev_end_io_acct(struct bio *bio, unsigned long start_time)
{
	struct gendisk *disk = bio->bi_bdev->bd_disk;
	if (blk_queue_io_stat(disk->queue)) {
		const int rw = bio_data_dir(bio);
		unsigned long duration = jiffies - start_time;
		int cpu = part_stat_lock();
		part_stat_add(cpu, &disk->part0, ticks[rw], duration);
		part_round_stats(cpu, &disk->part0);
		part_dec_in_flight(&disk->part0, rw);
		part_stat_unlock();
	}
}

static MRQTYPE
mdev_make_request(struct request_queue *q, struct bio *bio)
{
    struct mdev *md = q->queuedata;
    BIO_VEC_T bv;
    BVEC_ITER_T iter;
    unsigned long offset, start_time;
    sector_t sector;

    if (md == NULL) {
        BIO_ENDIO(bio, -EIO);
        MRQRETV;
    }
    if (BIO_HAS_DISCARD(bio)) {
        BIO_ENDIO(bio, 0);
        MRQRETV;
    }
    if (BIO_HAS_FLUSH(bio)) {
        BIO_ENDIO(bio, 0);
        MRQRETV;
    }
    start_time = jiffies;
    mdev_start_io_acct(bio);

    sector = BIO_SECTOR(bio);
    offset = sector << 9;

    bio_for_each_segment(bv, bio, iter) {
        unsigned int idx, off;
        unsigned char *sptr = page_address(BIOVEC(bv)->bv_page);
        unsigned char *dptr;
        unsigned long xf, this_xf, rf;

        xf = BIOVEC(bv)->bv_len;
        rf = BIOVEC(bv)->bv_offset;

        while (xf > 0) {
            idx = offset / ROW_SIZE;
            off = offset % ROW_SIZE;
            dptr = page_address(pgl[idx]);

            this_xf = xf;
            if (off + this_xf > ROW_SIZE)
                this_xf = ROW_SIZE - off;
            if (bio_data_dir(bio))
                memcpy(dptr + off, sptr + rf, this_xf);
            else
                memcpy(sptr + rf, dptr + off, this_xf);

            xf -= this_xf;
            rf += this_xf;
            offset += this_xf;
        }
    }
    BIO_ENDIO(bio, 0);
    mdev_end_io_acct(bio, start_time);

    MRQRETV;
}

struct mdev *
mdev_blksetup(void)
{
    struct mdev *md;

    md = kzalloc(sizeof(*md), GFP_KERNEL);
    if (!md)
        return (ERR_PTR(-ENOMEM));

    md->queue = blk_alloc_queue(GFP_KERNEL);
    if (!md->queue)
        return (ERR_PTR(-ENOMEM));

    md->queue->queue_flags = QUEUE_FLAG_DEFAULT;
    queue_flag_clear_unlocked(QUEUE_FLAG_STACKABLE, md->queue);
    queue_flag_set_unlocked(QUEUE_FLAG_NOMERGES, md->queue);
    queue_flag_set_unlocked(QUEUE_FLAG_NONROT, md->queue);
    blk_queue_make_request(md->queue, mdev_make_request);

    md->queue->queuedata = md;

    md->disk = alloc_disk(0);
    if (!md->disk) {
        blk_cleanup_queue(md->queue);
        kfree(md);
        return (ERR_PTR(-ENOMEM));
    }

    blk_queue_logical_block_size(md->queue, 1 << 9);

    blk_queue_max_hw_sectors(md->queue, DISK_SIZE >> 9);
    blk_queue_segment_boundary(md->queue, ROW_SIZE - 1);

    md->disk->major = mdev_bmajor;
    md->disk->fops = &mdev_bdfops;
    md->disk->private_data = md;
    md->disk->queue = md->queue;
    md->disk->flags = GENHD_FL_EXT_DEVT;
    sprintf(md->disk->disk_name, "mdev%d", 0);

    set_capacity(md->disk, DISK_SIZE >> 9);

    add_disk(md->disk);

    return (md);
}

static void
mdev_blkteardown(struct mdev *md)
{
    if (md->disk->flags & GENHD_FL_UP)
        del_gendisk(md->disk);
    if (!blk_queue_dead(md->queue))
        blk_cleanup_queue(md->queue);
    put_disk(md->disk);
    kfree(md);
}

static void
teardown(void)
{
    int i;

    if (!IS_ERR_OR_NULL(mdroot)) {
        mdev_blkteardown(mdroot);
    }
    for (i = 0; i < npgl; i++) {
        if (pgl[i])
            __free_pages(pgl[i], MDEV_ORDER);
    }
    if (!IS_ERR_OR_NULL(mdev_device))
        device_destroy(mdev_class, MKDEV(mdev_cmajor, 0));
    if (!IS_ERR_OR_NULL(mdev_class))
        class_destroy(mdev_class);
    __unregister_chrdev(mdev_cmajor, 0, 1, MODULE_NAME);
    unregister_blkdev(mdev_bmajor, "mdevblk");
    kfree(pgl);
}

static int
mdev_init(void)
{
    int i, rc;

    pgl = kzalloc(npgl * sizeof (struct page **), GFP_KERNEL);
    if (pgl == NULL) {
        pr_err("cannot allocate pagelist holders\n");
        return (-ENOMEM);
    }

    mdev_bmajor = register_blkdev(0, "mdevblk");
    if (mdev_bmajor < 0) {
        pr_err("cannot register blkdev (%d)\n", mdev_bmajor);
        return (mdev_bmajor);
    }

    mdev_cmajor = __register_chrdev(0, 0, 1, MODULE_NAME, &mdev_fops);
    if (mdev_cmajor < 0) {
        pr_err("cannot register chrdev (%d)\n", mdev_cmajor);
        unregister_blkdev(mdev_bmajor, "mdevblk");
        return (mdev_cmajor);
    }

    mdev_class = class_create(THIS_MODULE, "mdev");
    if (IS_ERR(mdev_class)) {
        rc = PTR_ERR(mdev_class);
        pr_err("cannot create sysfs class (%d)\n", rc);
        teardown();
        return (rc);
    }

    mdev_device = device_create(mdev_class, NULL, MKDEV(mdev_cmajor, 0), NULL, MODULE_NAME);
    if (IS_ERR(mdev_device)) {
        rc = PTR_ERR(mdev_device);
        pr_err("unable to create device (%d)\n", rc);
        teardown();
        return (rc);
    }

    for (i = 0; i < npgl; i++) {
        struct page *pages = alloc_pages(GFP_KERNEL | __GFP_ZERO, MDEV_ORDER);
        if (pages == NULL) {
            pr_err("unable to allocate local pagelist\n");
            teardown();
            return (-ENOMEM);
        }
        pgl[i] = pages;
    }

    mdroot = mdev_blksetup();
    if (IS_ERR(mdroot)) {
        teardown();
        return (PTR_ERR(mdroot));
    }

    return (0);
}

static void
mdev_exit(void)
{
    teardown();
}

module_init(mdev_init);
module_exit(mdev_exit);
MODULE_LICENSE("GPL");
