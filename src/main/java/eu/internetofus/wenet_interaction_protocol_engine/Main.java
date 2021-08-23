/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine;

import org.tinylog.Level;
import org.tinylog.provider.InternalLogger;

import eu.internetofus.common.vertx.AbstractMain;
import eu.internetofus.common.vertx.AbstractMainVerticle;

/**
 * Start the WeNet interaction protocol engine.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class Main extends AbstractMain {

  /**
   * Start the verticles.
   *
   * @param args arguments to configure the main process.
   *
   * @see MainVerticle
   */
  public static void main(final String... args) {

    final var main = new Main();
    main.startWith(args).onComplete(result -> {

      if (!result.succeeded()) {

        InternalLogger.log(Level.ERROR, result.cause(), "Can not start the WeNet interaction protocol engine!\n Check the Logs to known why.");
      }

    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getModuleName() {

    return "wenet-interaction-protocol-engine";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected AbstractMainVerticle createMainVerticle() {

    return new MainVerticle();
  }

}
