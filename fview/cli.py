# -*- coding: utf-8 -*-

import click
import fview

@click.command(name='fview')
@click.argument('target')
def main(target):
    """Console script for fview"""

    res = fview.read(target)
    click.echo(res)


if __name__ == "__main__":
    main()
