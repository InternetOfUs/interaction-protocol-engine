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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import org.junit.jupiter.api.Test;

/**
 * Test the {@link EngineWorker}.
 *
 * @see EngineWorker
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class EngineWorkerTest {

  /**
   * Should capture exception when copy resources.
   */
  @Test
  public void shouldCaptureExceptionWhneCopyResources() {

    final var worker = new EngineWorker();
    final Promise<Void> promise = Promise.promise();
    worker.copyResources(promise);
    assertThat(promise.future().cause()).isNotNull();

  }

  /**
   * Should capture error when handle {@code null} event.
   */
  @Test
  public void shouldCaptureErrorWhenHandleNullMessage() {

    final var worker = new EngineWorker();
    final io.vertx.core.eventbus.Message<JsonObject> event = null;
    worker.handle(event);

  }

  /**
   * Should capture error when handle empty event.
   */
  @Test
  public void shouldCaptureErrorWhenHandleEmptyMessage() {

    final var worker = new EngineWorker();
    @SuppressWarnings("unchecked")
    final io.vertx.core.eventbus.Message<JsonObject> event = mock(io.vertx.core.eventbus.Message.class);
    worker.handle(event);

  }

}
