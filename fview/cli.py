# -*- coding: utf-8 -*-

import click
import fview


@click.command(name='fview')
@click.argument('target')
def main(target):
    """Console script for fview"""

    parser = fview.read(target)

    click.echo(parser.display_contents())


if __name__ == "__main__":
    main()
